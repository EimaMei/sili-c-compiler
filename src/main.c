#include <sili.h>

#define SILEX_USE_HASH
#define SILEX_NO_LEN
#include <sililex.h>
#include <x86.h>



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
	scString name;
    scType type;
} scVariable;

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
    siArray(scVariable) parameters;
    siArray(scVariable) scope;
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

typedef SI_ENUM(u32, scInitializerType) {
	SC_INIT_BINARY,
	SC_INIT_CONSTANT,
	SC_INIT_IDENTIFIER
};

typedef struct {
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
} scInitializer;

typedef SI_ENUM(u32, scAstNodeType) {
	SC_AST_VAR_MAKE = 1,
};


typedef struct {
	scAstNodeType type;
	union {
		struct {
			scVariable* name;
			scInitializer* initializers;
		} var;
	} value;
} scAstNode;

typedef SI_ENUM(u32, scIndex) {
	SC_MAIN,

	SC_FILE,
	SC_FUNC,
	SC_VARS,

	SC_AST,
	SC_ASM,
	SC_X86ASM,

	SC_ALLOC_LEN
};


#define SC_MAX_FUNCS 128
#define SC_MAX_PARAM 128

#define SC_MAX_VARS 1024
#define SC_MAX_ACTIONS 160
#define SC_MAX_INITIALIZERS 32


typedef SI_ENUM(u32, scAsmType) {
	SC_ASM_PUSH_R64,
	SC_ASM_POP_R64,

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

	siAllocator* alloc[SC_ALLOC_LEN];

	cstring text;
	usize textLen;
	{
		siFile file = si_fileOpen("res/simple.c");
		alloc[SC_FILE] = si_allocatorMake(file.size);
		text = si_fileReadContents(file, alloc[SC_FILE]);
		textLen = file.size;

		si_fileClose(file);
	}

	alloc[SC_MAIN] = si_allocatorMake((sizeof(siArrayHeader) + sizeof(scAction) * SC_MAX_ACTIONS) * SC_MAX_FUNCS);
	alloc[SC_FUNC] = si_allocatorMake((sizeof(siArrayHeader) + sizeof(scFunction) + sizeof(scVariable) * SC_MAX_PARAM) * SC_MAX_FUNCS);
	alloc[SC_VARS] = si_allocatorMake(sizeof(scVariable) * SC_MAX_VARS + sizeof(scTokenStruct) * SC_MAX_INITIALIZERS * SC_MAX_ACTIONS + sizeof(siArrayHeader) * SC_MAX_ACTIONS);
	scLexer lex = silex_lexerMake(text, textLen);

	usize bytes = 0;
	for_range (i, 0, SC_ALLOC_LEN - 3) {
		bytes += alloc[i]->maxLen;
	}
	si_printf("%f MB\n", bytes / 1024.f / 1024.f);
	SI_ASSERT(bytes < SI_MEGA(1));

	scFunction scope_GLOBAL;
	scope_GLOBAL.scope = si_arrayMakeReserve(alloc[SC_VARS], sizeof(scVariable), 0);

	scFunction* curFunc = &scope_GLOBAL;
	scFunction* functions = si_arrayMakeReserve(alloc[SC_FUNC], sizeof(siFunction), 128);

	siTimeStamp ts = si_timeStampStart();
	while (silex_lexerTokenGet(&lex)) {
		switch (lex.type) {
			case SILEX_TOKEN_KEYWORD: {
				scType type = sc_typeGet(&lex);

				if (type.size != -1) {
					SI_ASSERT(lex.type == SILEX_TOKEN_IDENTIFIER);
					scString name = lex.token.text;

					b32 res = silex_lexerTokenGet(&lex);
					SI_ASSERT(res && lex.type == SILEX_TOKEN_PUNCTUATOR);

					switch (lex.token.punctuator) {
						case '(': {
							scFunction* func = &functions[si_arrayLen(functions)];
							func->type = type;
							func->name = name;
							func->parameters = si_arrayMakeReserve(alloc[SC_FUNC], sizeof(scVariable), 0);
							func->scope = si_arrayMakeReserve(alloc[SC_VARS], sizeof(scVariable), 0);
							func->code = si_arrayMakeReserve(alloc[SC_MAIN], sizeof(scAction), 0);

							while (res) {
								res = silex_lexerTokenGet(&lex);
								scType type = sc_typeGet(&lex);
								SI_ASSERT(res && lex.type == SILEX_TOKEN_IDENTIFIER);

								scVariable var;
								var.name = lex.token.text;
								var.type = type;

								si_arrayPush(&func->parameters, var);

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

							curFunc = func;

							break;
						}

						case '=': {
							scVariable var;
							var.name = name;
							var.type = type;
							scVariable* pVar = si_arrayPush(&curFunc->scope, var);

							scAction action;
							action.type = SC_ACTION_VAR_ASSIGN;
							action.values = si_arrayMakeReserve(alloc[SC_VARS], sizeof(scTokenStruct), 2);

							si_arrayPush(&action.values, pVar);
retry:
							res = silex_lexerTokenGet(&lex);
							SI_ASSERT(res);
							scTokenStruct token = (scTokenStruct){lex.type, lex.token};

							switch (lex.type) {
								case SILEX_TOKEN_PUNCTUATOR: {
									if (lex.token.punctuator == ';') {
										break;
									}
									SI_PANIC();
								}
								default:
									si_arrayPush(&action.values, token);
									goto retry;
							}

							si_arrayPush(&curFunc->code, action);
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
						action.values = si_arrayMakeReserve(alloc[SC_VARS], sizeof(scTokenStruct), 2);

						b32 res;
retry2_to_remove_later:
						res = silex_lexerTokenGet(&lex);
						SI_ASSERT(res);
						scTokenStruct token = (scTokenStruct){lex.type, lex.token};

						switch (lex.type) {
							case SILEX_TOKEN_PUNCTUATOR: {
								if (lex.token.punctuator == ';') {
									break;
								}
								SI_PANIC();
							}
							default:
								si_arrayPush(&action.values, token);
								goto retry2_to_remove_later;
						}

						si_arrayPush(&curFunc->code, action);
						break;
					}
				}
				break;
			}
		}
	}
	si_timeStampPrintSince(ts);

#if 1
	alloc[SC_AST] = si_allocatorMake((sizeof(scAstNode) + sizeof(siArrayHeader) + sizeof(scInitializer) * SC_MAX_INITIALIZERS) * si_arrayLen(curFunc->code));
	bytes = alloc[SC_AST]->maxLen;
	si_printf("%f MB\n", bytes / 1024.f / 1024.f);
	SI_ASSERT(bytes < SI_MEGA(1));

	siArray(scAstNode) ast = si_arrayMakeReserve(alloc[SC_AST], sizeof(scAstNode), si_arrayLen(curFunc->code));

	ts = si_timeStampStart();
	{
		SILEX_HASH_FUNC_INIT(hash_main);
		for_range (i, 0, 4) {
			SILEX_HASH_FUNC(hash_main, "main"[i]);
		}

		if (curFunc->name.hash != hash_main) {
			curFunc = nil;
			for_range (i, 0, si_arrayLen(functions)) {
				u64 hash = functions[i].name.hash;
				if (hash == hash_main) {
					curFunc = &functions[i];
				}
			}
		}
		SI_ASSERT_MSG(curFunc != nil, "main needs to exist");

		scType retType = curFunc->type;
		SI_ASSERT_MSG(retType.traits == SC_TYPE_INT && retType.size == 4, "'main' must return 'int'.");

		siArray(scVariable) args = curFunc->parameters;
		SI_ASSERT_MSG(si_arrayLen(args) == 0 || si_arrayLen(args) <= 2, "'main' has to either be empty or contain up to 2 parameters.");

		scType mainTypes[2];
		mainTypes[0] = type_int;
		mainTypes[1] = (scType){.size = 8, .ptr = &type_char, .ptrCount = 2, .traits = type_char.traits};
		for_range (i, 0, si_arrayLen(args)) {
			SI_ASSERT_FMT(sc_typeCmp(&args[i].type, &mainTypes[i]), "Argument %llz's type is incorrect", i);
		}
	}

	for_range (i, 0, si_arrayLen(curFunc->code)) {
		scAction* action = &curFunc->code[i];
		scAstNode node;

		switch (action->type) {
			case SC_ACTION_VAR_ASSIGN: {
				scVariable* var = *si_cast(scVariable**, &action->values[0]);
				node.type = SC_AST_VAR_MAKE;
				node.value.var.name = var;
				node.value.var.initializers = si_arrayMakeReserve(alloc[SC_AST], sizeof(scInitializer), 0);

				scTokenStruct* token1, *token2, *token3;
				scInitializer init;
				for_range (i, 1, si_arrayLen(action->values)) {
					token1 = &action->values[i];
					token2 = si_arrayAt(action->values, i + 1);

					switch (token1->type) {
						case SILEX_TOKEN_CONSTANT: {
							if (token2 != nil && token2->type == SILEX_TOKEN_OPERATOR) {
								token3 = si_arrayAt(action->values, i + 2);
								SI_ASSERT_MSG(token3 != nil, "Expected an expression after the operator.");

								init.type = SC_INIT_BINARY;
								init.value.binary.left = token1;
								init.value.binary.operator = token2->token.operator;
								init.value.binary.right = token3;
								i += 2;
								break;
							}

							init.type = SC_INIT_CONSTANT;
							init.value.constant = token1->token.constant;
							break;
						}

						default: SI_PANIC();
					}

					si_arrayPush(&node.value.var.initializers, init);
				}
				break;
			}
			default: continue;
		}

		si_arrayPush(&ast, node);
	}
	si_timeStampPrintSince(ts);
#endif

#if 1
	ts = si_timeStampStart();
	for_range (i, 0, si_arrayLen(ast)) {
		scAstNode* node = &ast[i];

		switch (node->type) {
			case SC_AST_VAR_MAKE: {
				siArray(scInitializer) initializers = node->value.var.initializers;
				for_range (j, 0, si_arrayLen(initializers)) {
					scInitializer* init = &initializers[j];

					switch (init->type) {
						case SC_INIT_BINARY: {
							scTokenStruct* left = init->value.binary.left,
										  *right = init->value.binary.right;
							if (left->type == SILEX_TOKEN_CONSTANT && left->type == right->type) {
								init->type = SC_INIT_CONSTANT;

								scConstant constant = left->token.constant;
								switch (init->value.binary.operator) {
									case SILEX_OPERATOR_PLUS: constant.value._signed += right->token.constant.value._signed; break;
									default: SI_PANIC();
								}
								init->value.constant = constant;
							}
						}
					}
				}

				break;
			}
		}

	}
	si_timeStampPrintSince(ts);
#endif


#if 1

	alloc[SC_ASM] = si_allocatorMake(sizeof(siArrayHeader) + si_arrayLen(ast) * sizeof(scAsm) + 3 * sizeof(scAsm) * si_arrayLen(functions));
	bytes = alloc[SC_ASM]->maxLen;
	si_printf("%f MB\n", bytes / 1024.f / 1024.f);
	SI_ASSERT(bytes < SI_MEGA(1));

	siArray(scAsm) instructions = si_arrayMakeReserve(alloc[SC_AST], sizeof(scAsm), si_arrayLen(ast) + 3 * si_arrayLen(functions));

	scAsm asm = {0};
	asm.type = SC_ASM_PUSH_R64;
	si_arrayPush(&instructions, asm);

	ts = si_timeStampStart();
	for_range (i, 0, si_arrayLen(ast)) {
		scAstNode* node = &ast[i];

		switch (node->type) {
			case SC_AST_VAR_MAKE: {
				scVariable* var = node->value.var.name;
				siArray(scInitializer) initializers = node->value.var.initializers;

				for_range (j, 0, si_arrayLen(initializers)) {
					scInitializer* init = &initializers[j];

					switch (init->type) {
						case SC_INIT_CONSTANT: {
							scConstant constant = init->value.constant;
							switch (var->type.size) {
								case 4: {
									curFunc->stack = si_alignCeilEx(curFunc->stack + 4, 4);
									asm.type = SC_ASM_LD_M32_I32;
									asm.dst = curFunc->stack;
									asm.src = constant.value._unsigned;

									break;
								}
								default: SI_PANIC();
							}
							break;
						}
						default: SI_PANIC();
					}
				}

				break;
			}
			default: SI_PANIC();
		}

		si_arrayPush(&instructions, asm);
	}

	si_timeStampPrintSince(ts);

	alloc[SC_X86ASM] = si_allocatorMake(sizeof(u8) * 4 * si_arrayLen(instructions));
	bytes = alloc[SC_X86ASM]->maxLen;
	si_printf("%f MB\n", bytes / 1024.f / 1024.f);
	SI_ASSERT(bytes < SI_MEGA(1));


	usize x86Len = 0;
	u8* x86 = si_mallocArray(alloc[SC_X86ASM], u8, 4 * si_arrayLen(instructions));

	ts = si_timeStampStart();

	for_range (i, 0, si_arrayLen(instructions)) {
		scAsm* instruction = &instructions[i];

		switch (instruction->type) {
			case SC_ASM_PUSH_R64: {
				break;
			}

			case SC_ASM_LD_M32_I32: {
				u8 out[8];
				usize len = sc_x86Opcode(
					X86_MOV_RM32_I32,
					0,
					instruction->dst,
					instruction->src,
					out
				);

				memcpy(&x86[x86Len], out, len);
				x86Len += len;
				break;
			}
		}
	}

	si_timeStampPrintSince(ts);
#endif
	for_range (i, 0, countof(alloc)) {
		si_allocatorFree(alloc[i]);
	}
}
