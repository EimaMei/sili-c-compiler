#include <sili.h>

#define SILEX_USE_HASH
#define SILEX_NO_LEN
#include <sililex.h>
#include <x86.h>
#include <exegen.h>


typedef SI_ENUM(u32, scInitializerType) {
	SC_INIT_BINARY = 1,
	SC_INIT_CONSTANT,
	SC_INIT_IDENTIFIER
};

typedef struct scInitializer {
	scInitializerType type;
	union {
		struct {
			scOperator operator;
			scTokenStruct* left;
			scTokenStruct* right;
		} binary;
		scConstant constant;
		scString identifier;
	} value;

	struct scInitializer* next;
} scInitializer;

typedef SI_ENUM(u32, scAstNodeType) {
	SC_AST_VAR_MAKE = 1,
	SC_AST_RETURN
};


typedef SI_ENUM(u32, scTypeTraits) {
	SC_TYPE_INT      = SI_BIT(0),
	SC_TYPE_UNSIGNED = SI_BIT(1),
	SC_TYPE_FLOAT    = SI_BIT(2),
	SC_TYPE_CONST    = SI_BIT(3),
	SC_TYPE_STATIC   = SI_BIT(4),
	SC_TYPE_FUNC_PTR = SI_BIT(5),
	SC_TYPE_ARRAY    = SI_BIT(6)
};

typedef struct scType {
	isize size;
	scTypeTraits traits;
	u32 ptrCount;
	struct scType* ptr;
} scType;

typedef struct {
	scType type;
	scInitializer* init;
} scVariable;
SI_STATIC_ASSERT(sizeof(scVariable) == 32);

typedef struct {
	scAstNodeType type;
	scInitializer* init;
	union {
		scVariable* var;
	} extra;
} scAstNode;

typedef SI_ENUM(u32, scActionType) {
	SC_ACTION_VAR_ASSIGN = 1,
	SC_ACTION_VAR_CREATE,
	SC_ACTION_RETURN,
};

typedef struct scAction {
	scActionType type;
	scTokenStruct* values;
} scAction;
SI_STATIC_ASSERT(sizeof(scAction) == 16);


typedef struct {
	scType type;
	scString name;
	siHashTable scope;
	siArray(u32) parameters;
	siArray(scAction) code;
	u32 stack;
} scFunction;


usize sizeof_SIZE_T = 8;


scType type_char = (scType){1, SC_TYPE_INT, 0, nil};
scType type_short = (scType){2, SC_TYPE_INT, 0, nil};
scType type_int = (scType){4, SC_TYPE_INT, 0, nil};
scType type_long = (scType){8, SC_TYPE_INT, 0, nil};
scType type_unsigned = (scType){4, SC_TYPE_INT | SC_TYPE_UNSIGNED, 0, nil};
scType type_float = (scType){4, SC_TYPE_FLOAT, 0, nil};
scType type_double = (scType){8, SC_TYPE_FLOAT, 0, nil};

#define sc_typeCmp(t1, t2) ((t1)->size == (t2)->size && SI_TO_U64((isize*)(t1) + 1) == SI_TO_U64((isize*)(t2) + 1))


#define sc_typeIsVoid(type) ((type)->size == 0)

scType* sc_typeGetFromKeyword(scKeyword keyword) {
	static scType* types[] = {
		&type_char, &type_short, &type_int, &type_long,
		&type_int, &type_unsigned,
		&type_float, &type_double
	};

	return types[keyword - SILEX_KEYWORD_CHAR];
}


scType sc_typeGet(scLexer* lex) {
	SI_ASSERT(lex->type == SILEX_TOKEN_KEYWORD);

	scKeyword keyword = lex->token.keyword;
	SI_STOPIF(!silex_keywordIsType(keyword), return (scType){.size = -1});

	scType* baseType = sc_typeGetFromKeyword(keyword);
	scType type = *baseType;

	b32 signedModifier = false;
	b32 res;
retry:
	res = silex_lexerTokenGet(lex);
	SI_ASSERT(res);

	switch (lex->type) {
		case SILEX_TOKEN_PUNCTUATOR: {
			scPunctuator punct = lex->token.punctuator;
			SI_ASSERT(punct == '*');

			if (type.ptrCount == 0) {
				type.ptr = baseType;
				type.size = sizeof_SIZE_T;
			}
			type.ptrCount += 1;
			goto retry;
		}

		case SILEX_TOKEN_KEYWORD: {
			switch (lex->token.keyword) {
				case SILEX_KEYWORD_CHAR:
				case SILEX_KEYWORD_SHORT:
				case SILEX_KEYWORD_INT:
				case SILEX_KEYWORD_LONG: {
					SI_ASSERT_MSG(si_betweenu(keyword, SILEX_KEYWORD_SIGNED, SILEX_KEYWORD_UNSIGNED), "You cannot have multiple types.");
					SI_ASSERT_MSG(signedModifier == false, "You cannot have multiple signed modifiers.");
					SI_ASSERT_MSG(type.traits & SC_TYPE_INT, "Signed modifiers cannot be used for non-integers.");

					type = *sc_typeGetFromKeyword(lex->token.keyword);
					if (keyword == SILEX_KEYWORD_UNSIGNED) {
						type.traits |= SC_TYPE_UNSIGNED;
					}
					goto retry;
				}

				case SILEX_KEYWORD_UNSIGNED: {
					SI_ASSERT_MSG(!si_betweenu(keyword, SILEX_KEYWORD_SIGNED, SILEX_KEYWORD_UNSIGNED), "You cannot have multiple signed modifiers.");
					SI_ASSERT_MSG(type.traits & SC_TYPE_INT, "Signed modifiers cannot be used for non-integers.");

					if (lex->token.keyword == SILEX_KEYWORD_UNSIGNED) {
						type.traits |= SC_TYPE_UNSIGNED;
					}

					goto retry;
				}

				default: SI_PANIC();
			}
			break;
		}
	}

	return type;
}


typedef SI_ENUM(u32, scIndex) {
	SC_FILE,
	SC_MAIN,

	SC_AST,
	SC_ASM,
	SC_X86ASM,
	SC_EXE,

	SC_ALLOC_LEN
};

siAllocator* alloc[SC_ALLOC_LEN];

#define SC_MAX_FUNCS 128

#define SC_MAX_VARS 128
#define SC_MAX_ACTIONS 128
#define SC_MAX_INITIALIZERS 32


typedef SI_ENUM(u32, scAsmType) {
	SC_ASM_PUSH_R64 = 1,
	SC_ASM_POP_R64,

	SC_ASM_LD_M8_FUNC_PARAM,
	SC_ASM_LD_M16_FUNC_PARAM,
	SC_ASM_LD_M32_FUNC_PARAM,
	SC_ASM_LD_M64_FUNC_PARAM,

	SC_ASM_LD_M8_I8,
	SC_ASM_LD_M16_I16,
	SC_ASM_LD_M32_I32,
	SC_ASM_LD_M64_I32,
	SC_ASM_LD_M64_I64,

	SC_ASM_RET_I32,
};

typedef struct {
	scAsmType type;
	u64 dst;
	u64 src;
} scAsm;

void sc_initializerConstantCalc(scInitializer* init, scOperator operator, scTokenStruct* right) {
	init->type = SC_INIT_CONSTANT;

	scConstant* constant = &init->value.constant;
	switch (operator) {
		case SILEX_OPERATOR_PLUS:
			constant->value.integer += right->token.constant.value.integer;
			break;
		case SILEX_OPERATOR_MINUS:
			constant->value.integer -= right->token.constant.value.integer;
			break;
		default: SI_PANIC();
	}
}

scPunctuator sc_actionAddValues(scLexer* lex, scAction* action) {
	scTokenStruct token;
	b32 res;
retry:
	res = silex_lexerTokenGet(lex);
	SI_ASSERT(res);
	token = (scTokenStruct){lex->type, lex->token};

	switch (lex->type) {
		case SILEX_TOKEN_PUNCTUATOR: {
			SI_ASSERT(lex->token.punctuator == ';' || lex->token.punctuator == ',');
			return lex->token.punctuator;
		}
		default:
			si_arrayPush(&action->values, token);
			goto retry;
	}

}

#define sc_actionEvaluate(action, node) \
	sc_actionEvaluateEx(action, node, 0)

void sc_actionEvaluateEx(scAction* action, scAstNode* node, usize i) {
	scInitializer* prevInit = nil;
	scTokenStruct* token1, *token2, *token3;

	for ( ; i < si_arrayLen(action->values); i += 1) {
		token1 = &action->values[i];
		token2 = si_arrayAt(action->values, i + 1);

		scInitializer* init = si_mallocItem(alloc[SC_AST], scInitializer);
		if (prevInit != nil) {
			prevInit->next = init;
		}
		else {
			node->init = init;
		}
		prevInit = init;
		init->next = nil;

		switch (token1->type) {
			case SILEX_TOKEN_CONSTANT: {
				if (token2 != nil) {
					SI_ASSERT(token2->type == SILEX_TOKEN_OPERATOR);
					token3 = si_arrayAt(action->values, i + 2);
					SI_ASSERT_MSG(token3 != nil, "Expected an expression after the operator.");

					init->type = SC_INIT_BINARY;
					init->value.binary.left = token1;
					init->value.binary.operator = token2->token.operator;
					init->value.binary.right = token3;
					i += 2;
					break;
				}

				init->type = SC_INIT_CONSTANT;
				init->value.constant = token1->token.constant;
				break;
			}

			case SILEX_TOKEN_OPERATOR: {
				SI_ASSERT(token2 != nil);
				init->type = SC_INIT_BINARY;
				init->value.binary.left = nil;
				init->value.binary.operator = token1->token.operator;
				init->value.binary.right = token2;

				i += 1;
				break;
			}
			case SILEX_TOKEN_IDENTIFIER: {
				if (token2 != nil) {
					si_printf("%i\n", token2->type);
					SI_ASSERT(token2->type == SILEX_TOKEN_OPERATOR);
					token3 = si_arrayAt(action->values, i + 2);
					SI_ASSERT_MSG(token3 != nil, "Expected an expression after the operator.");

					init->type = SC_INIT_BINARY;
					init->value.binary.left = token1;
					init->value.binary.operator = token2->token.operator;
					init->value.binary.right = token3;
					i += 2;
					break;
				}

				init->type = SC_INIT_IDENTIFIER;
				init->value.identifier = token1->token.identifier;
				break;
			}


			default: SI_PANIC();
		}
	}
}

#ifndef SILEX_HASH_FUNC_INIT
	#define SILEX_HASH_FUNC_INIT(hash) SILEX_HASH_TYPE hash = 14695981039346656037UL
#endif

#ifndef SILEX_HASH_FUNC
	#define SILEX_HASH_FUNC(hash, character) \
		do { \
			(hash) ^= (u64)(character); \
			(hash) *= 1099511628211UL; \
		} while (0)
	#endif

#ifndef SILEX_HASH_FUNC_END
	#define SILEX_HASH_FUNC_END(hash)
#endif


scVariable* sc_getVarAndOptimizeToken(scFunction* function, scTokenStruct* token) {
	scVariable* var = nil;

	if (token->type == SILEX_TOKEN_IDENTIFIER) {
		var = si_hashtableGetWithHash(function->scope, token->token.identifier.hash);
		if (var->init && var->init->type == SC_INIT_CONSTANT) {
			token->type = SILEX_TOKEN_CONSTANT;
			 token->token.constant = var->init->value.constant;
		}
	}

	return var;
}

int main(void) {
#if 0
	cstring keywords[] = {
		"auto", "break", "case", "char", "const", "continue", "default", "do",
		"double", "else", "enum", "extern", "float", "for", "goto", "if", "int", "long",
		"register", "return", "short", "signed", "sizeof", "static", "struct", "switch",
		"typedef", "union", "unsigned", "void", "volatile", "while"
	};
	u64 keywords_U64[countof(keywords)];

	for_range (i, 0, countof(keywords)) {
		usize len = si_cstrLen(keywords[i]);
		SI_ASSERT(len <= 8);
#if 0
		memcpy(&keywords_U64[i], keywords[i], len);

		char m[1024];
		memcpy(m, keywords[i], len + 1);
		si_cstrUpper(m);
		si_printf("SILEX_KEYWORD_%s,\n", m, m);
#endif
	}
#endif
#if 1

	cstring text;
	usize textLen;
	{
		siFile file = si_fileOpen("res/simple.c");
		alloc[SC_FILE] = si_allocatorMake(file.size);
		text = si_fileReadContents(file, alloc[SC_FILE]);
		textLen = file.size;

		si_fileClose(file);
	}
#endif

#if 1
	alloc[SC_MAIN] = si_allocatorMake(
		(3 * sizeof(siArrayHeader) + sizeof(siHashEntry) * SC_MAX_VARS) * SC_MAX_FUNCS +
		sizeof(scFunction) * SC_MAX_FUNCS +
		(sizeof(scVariable) * SC_MAX_VARS) * SC_MAX_FUNCS +
		(sizeof(scAction) * SC_MAX_ACTIONS) * SC_MAX_FUNCS +
		(sizeof(scTokenStruct) * SC_MAX_INITIALIZERS) * SC_MAX_ACTIONS
	);
	usize bytes = alloc[SC_MAIN]->maxLen;
	si_printf("%f MB\n", bytes / 1024.f / 1024.f);
	SI_ASSERT(bytes < SI_MEGA(2));

	scFunction scope_GLOBAL;
	scope_GLOBAL.scope = si_hashtableMakeReserve(alloc[SC_MAIN], SC_MAX_VARS);

	scFunction* curFunc = &scope_GLOBAL;
	siHashTable functions = si_hashtableMakeReserve(alloc[SC_MAIN], SC_MAX_FUNCS);
#endif

#if 1
	siTimeStamp ts = si_timeStampStart();
	scLexer lex = silex_lexerMake(text, textLen);
	while (silex_lexerTokenGet(&lex)) {
		switch (lex.type) {
			case SILEX_TOKEN_KEYWORD: {
				scType type = sc_typeGet(&lex);

				if (type.size != -1) {
					b32 isCreatingVar = false;
start:
					SI_ASSERT(lex.type == SILEX_TOKEN_IDENTIFIER);
					scString name = lex.token.identifier;

					b32 res = silex_lexerTokenGet(&lex);
					SI_ASSERT(res && lex.type == SILEX_TOKEN_PUNCTUATOR);

					switch (lex.token.punctuator) {
						case '(': {
							SI_ASSERT(isCreatingVar == false);

							scFunction* func = si_mallocItem(alloc[SC_MAIN], scFunction);
							func->type = type;
							func->name = name;
							func->scope = si_hashtableMakeReserve(alloc[SC_MAIN], SC_MAX_VARS);
							func->code = si_arrayMakeReserve(alloc[SC_MAIN], sizeof(scAction), 0);
							u32 paramaterIndex[128];

							while (res) {
								res = silex_lexerTokenGet(&lex);
								scType type = sc_typeGet(&lex);
								SI_ASSERT(res && lex.type == SILEX_TOKEN_IDENTIFIER);

								scVariable* pVar = si_mallocItem(alloc[SC_MAIN], scVariable);
								pVar->type = type;
								pVar->init = 0;

								siHashEntry* scopePtr = si_hashtableSetWithHash(func->scope, lex.token.identifier.hash, pVar);
								paramaterIndex[si_arrayLen(func->scope) - 1] = scopePtr - func->scope;

								res = silex_lexerTokenGet(&lex);
								SI_ASSERT(res && lex.type == SILEX_TOKEN_PUNCTUATOR);
								scPunctuator punc = lex.token.punctuator;

								if (punc == ',') {
									continue;
								}
								else if (punc == ')') {
									break;
								}
							}

							func->parameters = si_arrayMakeReserve(alloc[SC_MAIN], sizeof(u32), si_arrayLen(func->scope));
							SI_ARRAY_HEADER(func->parameters)->len = si_arrayLen(func->scope);
							for_range (i, 0, si_arrayLen(func->scope)) {
								func->parameters[i] = paramaterIndex[i];
							}

							si_hashtableSetWithHash(functions, name.hash, func);
							curFunc = func;

							break;
						}

						case '=': {
							scVariable* pVar = si_mallocItem(alloc[SC_MAIN], scVariable);
							pVar->type = type;

							scAction action;
							action.type = SC_ACTION_VAR_ASSIGN;
							action.values = si_arrayMakeReserve(alloc[SC_MAIN], sizeof(scTokenStruct), 2);
							si_arrayPush(&action.values, pVar);

							scPunctuator punc = sc_actionAddValues(&lex, &action);
							si_arrayPush(&curFunc->code, action);
							si_hashtableSetWithHash(curFunc->scope, name.hash, pVar);

							if (punc == ',') {
								if (type.ptr) {
									type = *type.ptr;
								}
								isCreatingVar = true;
								res = silex_lexerTokenGet(&lex);
								goto start;
							}
							break;
						}

						case ';':
							si_printf("Found a variable that exists.\n");
							break;
						default: SI_PANIC();
					}

					break;
				}

				switch (lex.token.keyword) {
					case SILEX_KEYWORD_RETURN: {
						scAction action;
						action.type = SC_ACTION_RETURN;
						action.values = si_arrayMakeReserve(alloc[SC_MAIN], sizeof(scTokenStruct), 1);

						sc_actionAddValues(&lex, &action);
						si_arrayPush(&curFunc->code, action);
						break;
					}
					default: SI_PANIC();
				}
				break;
			}
		}
	}
	si_timeStampPrintSince(ts);
#endif

#if 1
	alloc[SC_AST] = si_allocatorMake((sizeof(scAstNode) + sizeof(siArrayHeader) + sizeof(scInitializer) * SC_MAX_INITIALIZERS) * si_arrayLen(curFunc->code));
	bytes = alloc[SC_AST]->maxLen;
	si_printf("%f MB\n", bytes / 1024.f / 1024.f);
	SI_ASSERT(bytes < SI_MEGA(1));

	siArray(scAstNode) ast = si_arrayMakeReserve(alloc[SC_AST], sizeof(scAstNode), si_arrayLen(curFunc->code));

	ts = si_timeStampStart();
	SILEX_HASH_FUNC_INIT(hash_main);
	{
		for_range (i, 0, 4) {
			SILEX_HASH_FUNC(hash_main, "main"[i]);
		}

		if (curFunc->name.hash != hash_main) {
			curFunc = si_hashtableGetWithHash(functions, hash_main);
		}
		SI_ASSERT_MSG(curFunc != nil, "main needs to exist");

		scType retType = curFunc->type;
		SI_ASSERT_MSG(retType.traits == SC_TYPE_INT && retType.size == 4, "'main' must return 'int'.");

		siArray(u32) args = curFunc->parameters;
		SI_ASSERT_MSG(si_arrayLen(args) == 0 || si_arrayLen(args) <= 2, "'main' has to either be empty or contain up to 2 parameters.");

		scType mainTypes[2];
		mainTypes[0] = type_int;
		mainTypes[1] = (scType){.size = 8, .ptr = &type_char, .ptrCount = 2, .traits = type_char.traits};
		for_range (i, 0, si_arrayLen(args)) {
			scVariable* arg = curFunc->scope[args[i]].value;
			SI_ASSERT_FMT(sc_typeCmp(&arg->type, &mainTypes[i]), "Argument %llz's type is incorrect", i);
		}
	}

	for_range (i, 0, si_arrayLen(curFunc->code)) {
		scAction* action = &curFunc->code[i];
		scAstNode node;

		switch (action->type) {
			case SC_ACTION_VAR_ASSIGN: {
				scVariable* var = *si_cast(scVariable**, &action->values[0]);
				node.type = SC_AST_VAR_MAKE;
				node.extra.var = var;

				sc_actionEvaluateEx(action, &node, 1);
				var->init = node.init;
				break;
			}

			case SC_ACTION_RETURN: {
				node.type = SC_AST_RETURN;
				sc_actionEvaluate(action, &node);
				break;
			}
			default: SI_PANIC();
		}

		si_arrayPush(&ast, node);
	}
	si_timeStampPrintSince(ts);
#endif

#if 1
	ts = si_timeStampStart();

	for_range (i, 0, si_arrayLen(ast)) {
		scAstNode* node = &ast[i];
		scInitializer* prevInit = nil;

		scInitializer* inits = node->init;
		scInitializer* init = inits;

		while (init != nil) {
			switch (init->type) {
				case SC_INIT_BINARY: {
					scTokenStruct* left = init->value.binary.left,
								  *right = init->value.binary.right;

					if (left == nil) {
						switch (prevInit->type) {
							case SC_INIT_CONSTANT: {
								sc_initializerConstantCalc(
									prevInit, init->value.binary.operator, right
								);
								prevInit->next = init->next;

								break;
							}
							default: SI_PANIC();
						}
					}

					scVariable* leftVar = sc_getVarAndOptimizeToken(curFunc, left);
					scVariable* rightVar = sc_getVarAndOptimizeToken(curFunc, right);

					if (left->type == SILEX_TOKEN_CONSTANT && left->type == right->type) {
						scOperator op = init->value.binary.operator;
						init->value.constant = left->token.constant;

						sc_initializerConstantCalc(init, op, right);
						prevInit = init;
					}
					else { SI_PANIC(); }

					break;
				}
				case SC_INIT_IDENTIFIER: {
					scString identifier = init->value.identifier;
					scVariable* var = si_hashtableGetWithHash(curFunc->scope, identifier.hash);

					if (var->init && var->init->type == SC_INIT_CONSTANT) {
						init->type = SC_INIT_CONSTANT;
						init->value = var->init->value;
					}

					break;
				}
				case SC_INIT_CONSTANT: break;
				default: SI_PANIC();
			}

			init = init->next;
		}
	}
	si_timeStampPrintSince(ts);
#endif


#if 1

	alloc[SC_ASM] = si_allocatorMake(
		sizeof(siArrayHeader) + si_arrayLen(ast) * sizeof(scAsm) +
		3 * sizeof(scAsm) * si_arrayLen(functions) +
		si_arrayLen(curFunc->parameters) * sizeof(scAsm)
	);
	bytes = alloc[SC_ASM]->maxLen;
	si_printf("%f MB\n", bytes / 1024.f / 1024.f);
	SI_ASSERT(bytes < SI_MEGA(1));

	siArray(scAsm) instructions = si_arrayMakeReserve(alloc[SC_AST], sizeof(scAsm), si_arrayLen(ast) + 3 * si_arrayLen(functions));

	scAsm asm = {0};
	asm.type = SC_ASM_PUSH_R64;
	si_arrayPush(&instructions, asm);

	for_range (i, 0, si_arrayLen(curFunc->parameters)) {
		usize j = curFunc->parameters[i];
		scVariable* var = (scVariable*)curFunc->scope[j].value;
		asm.type = 0;

		switch (var->type.size) {
			case 8: {
				curFunc->stack = si_alignCeilEx(curFunc->stack + 8, 8);
				asm.type = SC_ASM_LD_M64_FUNC_PARAM;
				break;
			}
			case 4: {
				curFunc->stack = si_alignCeilEx(curFunc->stack + 4, 4);
				asm.type = SC_ASM_LD_M32_FUNC_PARAM;
				break;
			}

			default: SI_PANIC();
		}

		asm.dst = curFunc->stack;
		si_arrayPush(&instructions, asm);
	}

	ts = si_timeStampStart();
	for_range (i, 0, si_arrayLen(ast)) {
		scAstNode* node = &ast[i];

		switch (node->type) {
			case SC_AST_VAR_MAKE: {
				scVariable* var = node->extra.var;
				scInitializer* inits = node->init;

				scInitializer* init = inits;
				while (init != nil) {
					switch (init->type) {
						case SC_INIT_CONSTANT: {
							scConstant constant = init->value.constant;
							switch (var->type.size) {
								case 4: {
									curFunc->stack = si_alignCeilEx(curFunc->stack + 4, 4);
									asm.type = SC_ASM_LD_M32_I32;
									asm.dst = curFunc->stack;
									asm.src = constant.value.integer;

									break;
								}
								default: SI_PANIC();
							}
							break;
						}
						default: SI_PANIC();
					}
					init = init->next;
				}

				break;
			}
			case SC_AST_RETURN: {
				scInitializer* inits = node->init;

				scInitializer* init = inits;
				while (init != nil) {
					switch (init->type) {
						case SC_INIT_CONSTANT: {
							scConstant constant = init->value.constant;
							switch (curFunc->type.size) {
								case 4: {
									asm.type = SC_ASM_RET_I32;
									asm.src = constant.value.integer;

									break;
								}
								default: SI_PANIC();
							}
							break;
						}
						default: SI_PANIC();
					}
					init = init->next;
				}

				break;
			}

			default: SI_PANIC();
		}

		si_arrayPush(&instructions, asm);
	}

	si_timeStampPrintSince(ts);

	alloc[SC_X86ASM] = si_allocatorMake(sizeof(u8) * 16 * si_arrayLen(instructions));
	bytes = alloc[SC_X86ASM]->maxLen;
	si_printf("%f MB\n", bytes / 1024.f / 1024.f);
	SI_ASSERT(bytes < SI_MEGA(1));


	usize x86Len = 0;
	u8* x86 = si_mallocArray(alloc[SC_X86ASM], u8, 16 * si_arrayLen(instructions));
	x86EnvironmentState x86state;
	x86state.conv = X86_CALLING_CONV_SYSTEM_V_X86;
	x86state.registers = 0;

	ts = si_timeStampStart();

	for_range (i, 0, si_arrayLen(instructions)) {
		scAsm* instruction = &instructions[i];
		usize len;

		switch (instruction->type) {
			case SC_ASM_PUSH_R64: {
				x86[x86Len] = X86_PUSH_R64 + RBP;
				len = 1;
				len += sc_x86Opcode(
					X86_MOV_RM64_R64,
					X86_CFG_REX_PREFIX | X86_CFG_RMB | X86_CFG_64BIT | X86_CFG_DST_R | X86_CFG_SRC_R,
					RBP,
					RSP,
					&x86[x86Len + len]
				);

				x86Len += len;
				break;
			}

			case SC_ASM_LD_M32_FUNC_PARAM: {
				x86Register reg = sc_x86PickFunctionArg(&x86state);
				len = sc_x86Opcode(
					X86_MOV_RM32_R32,
					X86_CFG_RMB | X86_CFG_DST_M | X86_CFG_SRC_R,
					instruction->dst,
					reg,
					&x86[x86Len]
				);

				x86Len += len;
				break;
			}

			case SC_ASM_LD_M64_FUNC_PARAM: {
				x86Register reg = sc_x86PickFunctionArg(&x86state);
				len = sc_x86Opcode(
					X86_MOV_RM64_R64,
					X86_CFG_RMB | X86_CFG_64BIT | X86_CFG_DST_M | X86_CFG_SRC_R,
					instruction->dst,
					reg,
					&x86[x86Len]
				);

				x86Len += len;
				break;
			}


			case SC_ASM_LD_M32_I32: {
				len = sc_x86Opcode(
					X86_MOV_RM32_I32,
					X86_CFG_RMB | X86_CFG_ID | X86_CFG_DST_M,
					instruction->dst,
					instruction->src,
					&x86[x86Len]
				);

				x86Len += len;
				break;
			}

			case SC_ASM_RET_I32: {
				if (curFunc->name.hash != hash_main) {
					len = sc_x86Opcode(
						X86_MOV_R32_I32,
						X86_CFG_ID | X86_CFG_DST_R,
						RAX,
						instruction->src,
						&x86[x86Len]
					);
					x86[x86Len + len] = X86_POP_R64 + RBP;

					x86[x86Len + len + 1] = X86_RET;
					len += 2;
				}
				else {
					len = sc_x86Opcode(
						X86_MOV_RM32_I32,
						X86_CFG_RMB | X86_CFG_ID | X86_CFG_DST_R | X86_CFG_64BIT,
						RAX,
						60,
						&x86[x86Len]
					);
					len += sc_x86Opcode(
						X86_MOV_RM32_I32,
						X86_CFG_ID | X86_CFG_RMB | X86_CFG_DST_R,
						EDI,
						instruction->src,
						&x86[x86Len + len]
					);

					x86[x86Len + len] = X86_POP_R64 + RBP;
					x86[x86Len + len + 1] = (X86_SYSCALL & 0xFF00) >> 8;
					x86[x86Len + len + 2] = X86_SYSCALL & 0x00FF;
					len += 3;
				}

				x86Len += len;
				break;
			}

			default: SI_PANIC();
		}

		si_print("\t");
		for_range (j, 0, len) {
			si_printf("%ll02X ", x86[x86Len - len + j]);
		}
		si_print("\n");
	}

	si_timeStampPrintSince(ts);
#endif

#if 1
	ts = si_timeStampStart();

	usize elfSize = sizeof(elf64ElfHeader) + sizeof(elf64ProgramHeader);
	usize fileLen = elfSize + x86Len;

	alloc[SC_EXE] = si_allocatorMake(fileLen);
	u8* buf = si_mallocArray(alloc[SC_EXE], u8, fileLen);

	elf64_elfHeaderMake((elf64ElfHeader*)&buf[0], true, ELFOSABI_NONE, EM_X86_64);
	elf64_programHeaderMake((elf64ProgramHeader*)&buf[sizeof(elf64ElfHeader)], fileLen);
	memcpy(&buf[elfSize], x86, x86Len);

	siFile exe = si_fileCreate("a.out");
	si_fileWriteLen(&exe, buf, fileLen);
	si_fileClose(exe);

	si_timeStampPrintSince(ts);
#endif

	for_range (i, 0, countof(alloc)) {
		si_allocatorFree(alloc[i]);
	}
}
