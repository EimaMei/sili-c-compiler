#define SILEX_USE_HASH
#include <sili.h>
#include <sililex.h>

#include <scc.h>
#include <x86.h>
#include <exegen.h>


usize sizeof_SIZE_T = 8;

scType type_char     = (scType){1, SC_TYPE_INT, 0, nil};
scType type_short    = (scType){2, SC_TYPE_INT, 0, nil};
scType type_int      = (scType){4, SC_TYPE_INT, 0, nil};
scType type_long     = (scType){8, SC_TYPE_INT, 0, nil};
scType type_unsigned = (scType){4, SC_TYPE_INT | SC_TYPE_UNSIGNED, 0, nil};
scType type_float    = (scType){4, SC_TYPE_FLOAT, 0, nil};
scType type_double   = (scType){8, SC_TYPE_FLOAT, 0, nil};

siAllocator* alloc[SC_ALLOC_LEN];


#define SC_MAX_MACROS 512
#define SC_MAX_TYPES  512
#define SC_MAX_FUNCS  512
#define SC_MAX_VARS   512

#define SC_MAX_ACTIONS 512
#define SC_MAX_INITIALIZERS 64


scAsmType sc_asmGetCorrectType(scAsmType baseType, u32 typeSize) {
	switch (typeSize) {
		case 1: return baseType;
		case 2: return baseType + 1;
		case 4: return baseType + 2;
		case 8: return baseType + 3;
		case 16: return UINT32_MAX;
		default: return UINT32_MAX - 1;
	}
}

force_inline
scAsmType sc_asmGetCorrectOperator(scAsmType baseType, scOperator operator) {
	u32 index = operator - SILEX_OPERATOR_PLUS;
	return baseType + 4 * index;
}

force_inline
void x86_OP_M32_M32_EX(x86EnvironmentState* x86, scAsm* instruction, u8 opcode, x86Config bits);

force_inline
void x86_OP_M32_M32(x86EnvironmentState* x86, scAsm* instruction, u8 opcode) {
	x86_OP_M32_M32_EX(x86, instruction, opcode, 0);
}

force_inline
void x86_OP_M32_M32_EX(x86EnvironmentState* x86, scAsm* instruction, u8 opcode, x86Config bits) {
	x86Register reg = sc_x86PickAvailableReg(x86);

	sc_x86Opcode(
		x86, X86_MOV_R32_RM32, reg, instruction->src,
		X86_CFG_RMB | X86_CFG_DST_R | X86_CFG_SRC_M
	);
	sc_x86Opcode(
		x86, opcode, instruction->dst, reg,
		X86_CFG_RMB | X86_CFG_DST_M | X86_CFG_SRC_R | bits
	);
}


#define X86_ASM_TEMPLATE_RMB_EX(x86, type, baseOpcode, instruction, extra, extra8bit, extra16bit, extra32bit, extra64bit) \
	case (type): { \
		x86Register reg = sc_x86RegisterConvert((x86), (instruction)->dst); \
		sc_x86Opcode( \
			x86, baseOpcode, reg, (instruction)->src, \
			X86_CFG_RMB | (extra) |(extra8bit) \
		); \
		break; \
	} \
	case (type + 1): { \
		x86Register reg = sc_x86RegisterConvert((x86), (instruction)->dst); \
		sc_x86Opcode( \
			x86, baseOpcode, reg, (instruction)->src, \
			X86_CFG_RMB | (extra) | (extra16bit) \
		); \
		break; \
	} \
	case (type) + 2: { \
		x86Register reg = sc_x86RegisterConvert((x86), (instruction)->dst); \
		sc_x86Opcode( \
			x86, (baseOpcode) + 1, reg, (instruction)->src, \
			X86_CFG_RMB | (extra) | (extra32bit) \
		); \
		break; \
	} \
	case (type) + 3: { \
		x86Register reg = sc_x86RegisterConvert((x86), (instruction)->dst); \
		sc_x86Opcode( \
			x86, (baseOpcode) + 1, reg, (instruction)->src, \
			X86_CFG_RMB |  X86_CFG_64BIT | (extra) | (extra64bit) \
		); \
		break; \
	} \

#define X86_ASM_TEMPLATE_RMB(x86, type, baseOpcode, instruction, extra) \
	X86_ASM_TEMPLATE_RMB_EX(x86, type, baseOpcode, instruction, extra, 0, 0, 0, 0)

#define X86_ASM_TEMPLATE_R_M(x86, type, baseOpcode, instruction) \
	X86_ASM_TEMPLATE_RMB(x86, type, baseOpcode, instruction, X86_CFG_DST_R | X86_CFG_DST_M)

#define X86_ASM_TEMPLATE_M_I(x86, type, baseOpcode, instruction) \
	X86_ASM_TEMPLATE_RMB_EX(x86, type, baseOpcode, instruction, X86_CFG_DST_M, X86_CFG_IB, X86_CFG_IW, X86_CFG_ID, X86_CFG_ID)



#if 0
switch (node->type) {
	case SC_AST_VAR_MAKE: {
							  scVariable* var = (scVariable*)node->key->identifier;
							  scInitializer* inits = node->init;

							  scInitializer* init = inits;
							  stack = si_alignCeilEx(stack + var->type.size, var->type.size);
							  var->location = stack;
#endif
void sc_astNodeToAsm(scInfoTable* scope, scAsm* instructions, scOperator assignment,
		scAsmType asmTypes[3], b32 useRegForBinary, u32 typeSize, scAstNode* node,
		u32 regSrc) {
	scAsm asm;

	scInitializer* init = node->init;

	switch (init->type) {
		case SC_INIT_CONSTANT: {
			scConstant constant = init->value.constant;

			/* TODO(EimaMei): SC_ASM_LD_M64_I64. */
			asm.type = sc_asmGetCorrectType(asmTypes[0], typeSize);
			asm.src = constant.value.integer;

			if (node->key) {
				scIdentifierKey* key = node->key;
				SI_ASSERT(key->type == SC_IDENTIFIER_KEY_VAR);
				scVariable* var = (scVariable*)key->identifier;
				asm.dst = var->location;
			}
			else {
				asm.dst = 0;
			}

			si_arrayPush(&instructions, asm);
			break;
		}
		case SC_INIT_IDENTIFIER: {
			u64 hash = init->value.identifier;
			scIdentifierKey* key = si_hashtableGetWithHash(scope->identifiers, hash);
			SI_ASSERT(key->type != SC_IDENTIFIER_KEY_FUNC);

			scVariable* var = (scVariable*)key->identifier;

			asm.type = sc_asmGetCorrectType(asmTypes[1], var->type.size);
			asm.src = var->location;

			if (node->key) {
				scIdentifierKey* key = node->key;
				SI_ASSERT(key->type == SC_IDENTIFIER_KEY_VAR);
				scVariable* var = (scVariable*)key->identifier;
				asm.dst = var->location;
			}
			else {
				asm.dst = 0;
			}

			si_arrayPush(&instructions, asm);
			break;
		}
		case SC_INIT_BINARY: {
			scTokenStruct* left, *right;
start:
			left = init->value.binary.left,
			right = init->value.binary.right;
			scOperator operator = init->value.binary.operator;

			scTokenStruct* arguments[2] = {left, right};

			scAsmType typesLD[2];
			scAsmType typesOP[2];
			if (!useRegForBinary) {
				typesLD[0] = !assignment ? SC_ASM_LD_M8_M8 : sc_asmGetCorrectOperator(SC_ASM_ADD_M8_M8, assignment);
				typesLD[1] = !assignment ? SC_ASM_LD_M8_I8 : sc_asmGetCorrectOperator(SC_ASM_ADD_M8_I8, assignment);
				typesOP[0] = sc_asmGetCorrectOperator(SC_ASM_ADD_M8_M8, operator);
				typesOP[1] = sc_asmGetCorrectOperator(SC_ASM_ADD_M8_I8, operator);
			}
			else {
				typesLD[0] = !assignment ? SC_ASM_LD_R8_M8 : sc_asmGetCorrectOperator(SC_ASM_ADD_R8_M8, assignment);
				typesLD[1] = !assignment ? SC_ASM_LD_R8_I8 : sc_asmGetCorrectOperator(SC_ASM_ADD_R8_I8, assignment);
				typesOP[0] = sc_asmGetCorrectOperator(SC_ASM_ADD_R8_M8, operator);
				typesOP[1] = sc_asmGetCorrectOperator(SC_ASM_ADD_R8_I8, operator);
			}

			scAsmType* types[2] = {typesLD, typesOP};
			/* NOTE(EimaMei): '(left == nil)' ignoruoja pirmąjį argumentą, jei jis
			 * yra nežinomasis. */
			for_range (j, (left == nil), countof(arguments)) {
				scTokenStruct* arg = arguments[j];

				switch (arg->type) {
					case SILEX_TOKEN_IDENTIFIER: {
						u64 hash = arg->token.identifier.hash;
						scIdentifierKey* key = si_hashtableGetWithHash(scope->identifiers, hash);
						SI_ASSERT(key->type != SC_IDENTIFIER_KEY_FUNC);

						scVariable* var = (scVariable*)key->identifier;
						asm.type = sc_asmGetCorrectType(types[j][0], var->type.size);
						asm.src = var->location;

						break;
					}
					case SILEX_TOKEN_CONSTANT: {
						scConstant constant = arg->token.constant;

						asm.type = sc_asmGetCorrectType(types[j][1], typeSize);
						asm.src = constant.value.integer;

						break;
					}
				}

				if (node->key) {
					scIdentifierKey* key = node->key;
					SI_ASSERT(key->type == SC_IDENTIFIER_KEY_VAR);

					scVariable* var = (scVariable*)key->identifier;
					asm.dst = var->location;
				}
				else {
					asm.dst = 0;
				}

				si_arrayPush(&instructions, asm);
			}

			break;
		}
	}

	init = init->next;
	SI_STOPIF(init != nil, goto start);


	if (useRegForBinary) {
		asm.type = sc_asmGetCorrectType(asmTypes[2], typeSize);
		asm.dst = 0;
		asm.src = regSrc;
		si_arrayPush(&instructions, asm);
	}
}


void sc_parseFunction(scInfoTable* scope, scFunction* func, scAsm* instructions) {
	SI_LOG("== Parsing the function ==\n");
	siArray(scAstNode) ast = si_arrayMakeReserve(alloc[SC_AST], sizeof(scAstNode), si_arrayLen(func->code));

	for_range (i, 0, si_arrayLen(func->code)) {
		scAction* action = &func->code[i];
		scAstNode node;

		switch (action->type) {
			case SC_ACTION_VAR_ASSIGN: {
				scIdentifierKey* key = *si_cast(scIdentifierKey**, &action->values[0]);
				scVariable* var = (scVariable*)key->identifier;

				node.type = SC_AST_VAR_MAKE;
				node.key = key;

				sc_actionEvaluateEx(action, &node, 1);
				var->init = node.init;
				break;
			}
			case SC_ACTION_VAR_ADD: {
				scIdentifierKey* key = *si_cast(scIdentifierKey**, &action->values[0]);
				scVariable* var = (scVariable*)key->identifier;

				node.type = SC_AST_VAR_ADD;
				node.key = key;

				sc_actionEvaluateEx(action, &node, 1);
				var->init = node.init;
				break;
			}
			case SC_ACTION_VAR_SUB: {
				scIdentifierKey* key = *si_cast(scIdentifierKey**, &action->values[0]);
				scVariable* var = (scVariable*)key->identifier;

				node.type = SC_AST_VAR_SUB;
				node.key = key;

				sc_actionEvaluateEx(action, &node, 1);
				var->init = node.init;
				break;
			}

			case SC_ACTION_RETURN: {
				node.type = SC_AST_RETURN;
				node.key = nil;
				sc_actionEvaluate(action, &node);
				break;
			}
			default: SI_PANIC();
		}

		si_arrayPush(&ast, node);
	}
	SI_LOG("== scAction -> scAst complete  ==\n");

	scInitializer* prevInit;
	scTokenStruct* prevTokenStruct[2];

	for_range (i, 0, si_arrayLen(ast)) {
		scAstNode* node = &ast[i];
		prevInit = nil;

		scInitializer* start = node->init;
		scInitializer* init = start;

		while (init != nil) {
			switch (init->type) {
				case SC_INIT_BINARY: {
					scTokenStruct* left = init->value.binary.left,
								  *right = init->value.binary.right;

					if (left == nil) {
						switch (prevInit->type) {
							case SC_INIT_CONSTANT: {
								scConstant constant = prevInit->value.constant;
								left = init->value.binary.left = prevTokenStruct[0];

								left->type = SILEX_TOKEN_CONSTANT;
								left->token.constant = constant;

								if (start == prevInit) {
									node->init = init;
								}
								else  {
									*prevInit = *init;
								}

								break;
							}
							case SC_INIT_BINARY: {
								for_range (i, 0, countof(prevTokenStruct)) {
									left = prevTokenStruct[i];

									if (left->type == SILEX_TOKEN_CONSTANT && left->type == right->type) {
										scOperator operator = init->value.binary.operator;
										switch (operator) {
											case SILEX_OPERATOR_PLUS:
												left->token.constant.value.integer += right->token.constant.value.integer;
												break;
											case SILEX_OPERATOR_MINUS:
												left->token.constant.value.integer -= right->token.constant.value.integer;
												break;
											/* case SILEX_OPERATOR_MUL: oppositeLeft *= right; */
											default: SI_PANIC();
										}
										prevInit->next = init->next;
										init = prevInit;
										break;
									}
								}
								i32 err;
								sc_variableGetAndOptimizeToken(scope, right, &err);
								sc_variableErrorCheck(err);

								prevInit = init;
								init = init->next;

								continue;
							}
							default: SI_PANIC();
						}
					}
					else {
						i32 err;
						scVariable* leftVar = sc_variableGetAndOptimizeToken(scope, left, &err);
						sc_variableErrorCheck(err);
					}
					i32 err;
					scVariable* rightVar = sc_variableGetAndOptimizeToken(scope, right, &err);
					sc_variableErrorCheck(err);

					prevTokenStruct[0] = left;
					prevTokenStruct[1] = right;

					if (left->type == SILEX_TOKEN_CONSTANT && left->type == right->type) {
						scOperator op = init->value.binary.operator;
						init->value.constant = left->token.constant;

						sc_constantArithmetic(&left->token.constant, op, right->token.constant);
						init->type = SC_INIT_CONSTANT;
						init->value.constant = left->token.constant;
					}

					break;
				}
				case SC_INIT_IDENTIFIER: {
					u64 hash = init->value.identifier;
					i32 err;
					scVariable* var = sc_variableGet(scope, hash, &err);
					sc_variableErrorCheck(err);

					if (var->init && var->init->type == SC_INIT_CONSTANT) {
						//SI_PANIC();
						//init->type = SC_INIT_CONSTANT;
						//init->value = var->init->value;
					}

					break;
				}
				case SC_INIT_CONSTANT: break;
				default: SI_PANIC();
			}

			prevInit = init;
			init = init->next;
		}
	}
	SI_LOG("== scAst optimizations complete  ==\n");


	scAsm asm;
	asm.type = SC_ASM_FUNC_START;
	asm.src = func - global_scope.funcs;
	si_arrayPush(&instructions, asm);

	asm.type = SC_ASM_PUSH_R64;
	si_arrayPush(&instructions, asm);

	usize stack = 0;
	for_range (i, 0, func->paramLen) {
		usize j = func->paramVars[i];

		scIdentifierKey* key = scope->identifiers[j].value;
		scVariable* var = (scVariable*)key->identifier;

		stack = si_alignCeilEx(stack + var->type.size, var->type.size);
		var->location = stack;

		asm.type = sc_asmGetCorrectType(SC_ASM_LD_M8_PARAM, var->type.size);
		asm.dst = stack;
		/* asm.src = 0; */

		si_arrayPush(&instructions, asm);
	}
	SI_LOG("== Parameters -> scAsm complete  ==\n");

	for_range (i, 0, si_arrayLen(ast)) {
		scAstNode* node = &ast[i];

		switch (node->type) {
			case SC_AST_VAR_MAKE: {
				scVariable* var = (scVariable*)node->key->identifier;
				stack = si_alignCeilEx(stack + var->type.size, var->type.size);
				var->location = stack;

				sc_astNodeToAsm(
					scope, instructions, 0,
					si_buf(u32, SC_ASM_LD_M8_I8, SC_ASM_LD_M8_M8), false,
					var->type.size, node,
					0
				);
				break;
			}
			case SC_AST_VAR_ADD: {
				scVariable* var = (scVariable*)node->key->identifier;

				sc_astNodeToAsm(
					scope, instructions, SILEX_OPERATOR_PLUS,
					si_buf(u32, SC_ASM_ADD_M8_I8, SC_ASM_ADD_M8_M8), false,
					var->type.size, node,
					0
				);
				break;
			}
			case SC_AST_VAR_SUB: {
				scVariable* var = (scVariable*)node->key->identifier;

				sc_astNodeToAsm(
					scope, instructions, SILEX_OPERATOR_MINUS,
					si_buf(u32, SC_ASM_SUB_M8_I8, SC_ASM_SUB_M8_M8), false,
					var->type.size, node,
					0
				);
				break;
			}
			case SC_AST_RETURN: {
				sc_astNodeToAsm(
					scope, instructions, 0,
					si_buf(u32, SC_ASM_RET_I8, SC_ASM_RET_M8, SC_ASM_RET_R8), node->init->type == SC_INIT_BINARY,
					func->type.size, node,
					SC_RETURN_REGISTER
				);
				break;
			}
			default: SI_PANIC();
		}
	}
	SI_LOG("== Parse complete  ==\n");
}


void sc_functionValidateMain(scFunction* func) {
	scType retType = func->type;
	SI_ASSERT_MSG(retType.traits == SC_TYPE_INT && retType.size == 4, "'main' must return 'int'.");

	SI_ASSERT_MSG(si_betweenu(func->paramLen, 0, 2), "A 'main' function can only contain up to 2 parameters.");

	scType mainTypes[2];
	mainTypes[0] = type_int; /* int argc */
	mainTypes[1] = (scType){ /* char** argv */
		.size = 8, .ptr = &type_char, .ptrCount = 2, .traits = type_char.traits
	};

	for_range (i, 0, func->paramLen) {
		scType* type = &func->paramTypes[i];
		SI_ASSERT_FMT(sc_typeCmp(type, &mainTypes[i]), "Argument %llz's type is incorrect", i + 1);
	}
}

scIdentifierKey* sc_identifierKeyGet(scInfoTable* scope, u64 name, scIdentifierKeyType type) {
	b32 res;
	siHashEntry* entry = si_hashtableSetWithHash(scope->identifiers, name, nil, &res);

	scIdentifierKey* key;
	if (res == false) { /* An identifier already exists in the list. */
		key = entry->value;
		SI_ASSERT_MSG(scope->rank < key->rank, "An identifier already exists.");
	}
	else { /* Nothing is occupied. */
		usize size;
		switch (type) {
			case SC_IDENTIFIER_KEY_VAR: size = sizeof(scVariable); break;
			case SC_IDENTIFIER_KEY_FUNC: size = sizeof(scFunction); break;
			case SC_IDENTIFIER_KEY_TYPE: size = sizeof(scType); break;
			default: SI_PANIC();
		}
		key = si_malloc(alloc[SC_MAIN], sizeof(scIdentifierKey) + size);
		entry->value = key;
	}
	key->type = type;
	key->rank = scope->rank;

	return key;
}

scGlobalInfoTable global_scope;
u64 hash_main;


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
	SC_ALLOCATOR_MAKE(
		SC_MAIN,
		SI_MEGA(1),
		(3 * sizeof(siArrayHeader) + sizeof(siHashEntry) * SC_MAX_VARS) +

		sizeof(scIdentifierKey) * (SC_MAX_VARS + SC_MAX_TYPES + SC_MAX_TYPES + SC_MAX_MACROS) +
		sizeof(scFunction) * SC_MAX_FUNCS +
		sizeof(scVariable) * SC_MAX_VARS +
		sizeof(scType) * SC_MAX_TYPES +
		sizeof(scVariable) * SC_MAX_MACROS +

		sizeof(scAction) * SC_MAX_ACTIONS +
		sizeof(scTokenStruct) * SC_MAX_INITIALIZERS * SC_MAX_ACTIONS
	);
	SC_ALLOCATOR_MAKE(
		SC_AST,
		USIZE_MAX,
		(sizeof(scAstNode) + sizeof(siArrayHeader) + sizeof(scInitializer) * SC_MAX_INITIALIZERS) * SC_MAX_ACTIONS
	);

	SC_ALLOCATOR_MAKE(
		SC_ASM,
		sizeof(siArrayHeader) + SC_MAX_INITIALIZERS * SC_MAX_ACTIONS * sizeof(scAsm),
		sizeof(siArrayHeader) + SC_MAX_INITIALIZERS * SC_MAX_ACTIONS * sizeof(scAsm)
	);

	SC_ALLOCATOR_MAKE(
		SC_SCOPE,
		SI_KILO(1),
		SI_KILO(1)
	);
	siArray(scAsm) asm = si_arrayMakeReserve(alloc[SC_ASM], sizeof(scAsm), SC_MAX_INITIALIZERS * SC_MAX_ACTIONS);

	global_scope.parent = nil;
	global_scope.identifiers = si_hashtableMakeReserve(
		alloc[SC_MAIN], SC_MAX_VARS + SC_MAX_FUNCS + SC_MAX_MACROS + SC_MAX_TYPES
	);

	global_scope.scopeRank = 0;
	global_scope.mainFuncID = 0;

	global_scope.varsLen = 0;
	global_scope.vars = si_malloc(
		alloc[SC_MAIN],
		sizeof(scIdentifierKey) * SC_MAX_VARS +
		sizeof(scVariable) * SC_MAX_VARS
	);
	global_scope.typesLen = 0;
	global_scope.types = si_malloc(
		alloc[SC_MAIN],
		sizeof(scIdentifierKey) * SC_MAX_TYPES +
		sizeof(scType) * SC_MAX_TYPES
	);

	global_scope.funcsLen = 0;
	global_scope.funcs = si_malloc(
		alloc[SC_MAIN],
		sizeof(scIdentifierKey) * SC_MAX_FUNCS +
		sizeof(scFunction) * SC_MAX_FUNCS
	);

	scInfoTable* scope = (scInfoTable*)&global_scope;
	scFunction* curFunc = nil;

    hash_main = 14695981039346656037UL;
    for_range (i, 0, 4) {
		SILEX_HASH_FUNC(hash_main, "main"[i]);
	}

#endif

#if 0 /* TESTING THE PERFORMANCE OF si_hashtableGet */
	u64 hash = 0x8ED4B07B589FB77;
	for_range (i, 0, 1023) {
		u64 n = rand();
		while (n == hash) {
			n = rand();
		}
		si_hashtableSetWithHash(functions, n, nil);
	}
	si_hashtableSetWithHash(functions, hash, &hash);

	siTimeStamp ts2 = si_timeStampStart();

	u64* n;
	for_range (i, 1, 1000000) {
		n = si_hashtableGetWithHash(functions, 0x8ED4B07B589FB77);
	}
	si_timeStampPrintSince(ts2);

	si_printf("%ll#X: %p %ll#X\n", (n) ? *n : 0, n, hash);
	SI_PANIC();
#endif

#if 1
	siTimeStamp ts = si_timeStampStart();
	scLexer lex = silex_lexerMake(text, textLen);
	scType* baseType;
	b32 res, commaMode = false;
	scKeyword keyword;
	scAction action;

	while (silex_lexerTokenGet(&lex)) {
		switch (lex.type) {
			case SILEX_TOKEN_IDENTIFIER: {
				scString identifier = lex.token.identifier;
				scIdentifierKey* key = si_hashtableGetWithHash(scope->identifiers, identifier.hash);

				SI_ASSERT_FMT(
					key != nil && key->rank <= scope->rank,
					"Type '%*s' doesn't exist", identifier.len, lex.curData - identifier.len
				);

				switch (key->type) {
					case SC_IDENTIFIER_KEY_TYPE: {
						baseType = (scType*)key->identifier;
						keyword = 0;
						goto type_section;
					}
					case SC_IDENTIFIER_KEY_VAR: {
						res = silex_lexerTokenGet(&lex);
						SI_ASSERT(res);

						switch (lex.type) {
							case SILEX_TOKEN_OPERATOR: {
								switch (lex.token.operator) {
									case SILEX_OPERATOR_PLUS_PLUS: {
										action.type = SC_ACTION_VAR_ADD;
										action.values = si_arrayMakeReserve(alloc[SC_MAIN], sizeof(scTokenStruct), 2);
										si_arrayPush(&action.values, key);

										scTokenStruct* t = &action.values[1];
										t->type = SILEX_TOKEN_CONSTANT;
										t->token.constant.type = SILEX_CONSTANT_NUM_SIGNED;
										t->token.constant.value.integer = 1;
										SI_ARRAY_HEADER(action.values)->len = 2;

										b32 res = silex_lexerTokenGet(&lex);
										SI_ASSERT(res && lex.type == SILEX_TOKEN_PUNCTUATOR);
										si_arrayPush(&curFunc->code, action);

										switch (lex.token.punctuator) {
											case ',': case ';': break;
											default: SI_PANIC_MSG("Expression should end with a semicolin or continued via a comma punctuator.");
										}
										break;
									}
									case SILEX_OPERATOR_MINUS_MINUS: {
										action.type = SC_ACTION_VAR_SUB;
										action.values = si_arrayMakeReserve(alloc[SC_MAIN], sizeof(scTokenStruct), 2);
										si_arrayPush(&action.values, key);

										scTokenStruct* t = &action.values[1];
										t->type = SILEX_TOKEN_CONSTANT;
										t->token.constant.type = SILEX_CONSTANT_NUM_SIGNED;
										t->token.constant.value.integer = 1;
										SI_ARRAY_HEADER(action.values)->len = 2;

										b32 res = silex_lexerTokenGet(&lex);
										SI_ASSERT(res && lex.type == SILEX_TOKEN_PUNCTUATOR);
										si_arrayPush(&curFunc->code, action);

										switch (lex.token.punctuator) {
											case ',': case ';': break;
											default: SI_PANIC_MSG("Expression should end with a semicolin or continued via a comma punctuator.");
										}
										break;
									}
									case SILEX_OPERATOR_PLUS_ASSIGN: {
										action.type = SC_ACTION_VAR_ADD;
										action.values = si_arrayMakeReserve(alloc[SC_MAIN], sizeof(scTokenStruct), 2);
										si_arrayPush(&action.values, key);

										scPunctuator punc = sc_actionAddValues(&lex, &action);
										si_arrayPush(&curFunc->code, action);

										switch (punc) {
											case ',': case ';': break;
											default: SI_PANIC_MSG("Expression should end with a semicolin or continued via a comma punctuator.");
										}
										break;
									}
									case SILEX_OPERATOR_MINUS_ASSIGN: {
										action.type = SC_ACTION_VAR_SUB;
										action.values = si_arrayMakeReserve(alloc[SC_MAIN], sizeof(scTokenStruct), 2);
										si_arrayPush(&action.values, key);

										scPunctuator punc = sc_actionAddValues(&lex, &action);
										si_arrayPush(&curFunc->code, action);

										switch (punc) {
											case ',': case ';': break;
											default: SI_PANIC_MSG("Expression should end with a semicolin or continued via a comma punctuator.");
										}
										break;
									}

									default: SI_PANIC();
								}
								break;
							}
							default: SI_PANIC();
						}
						break;
					}
					default: SI_PANIC();
				}



				break;
			}
			case SILEX_TOKEN_KEYWORD: {
				keyword = lex.token.keyword;
				SI_STOPIF(!silex_keywordIsType(keyword), goto keyword_section);

				baseType = sc_typeGetFromKeyword(keyword);
type_section:
				SI_ASSERT(commaMode == false);
				scType type = sc_typeMake(&lex, baseType, keyword);
type_section_start:
				SI_ASSERT(lex.type == SILEX_TOKEN_IDENTIFIER);
				u64 name = lex.token.identifier.hash;

				res = silex_lexerTokenGet(&lex);
				SI_ASSERT(res && lex.type == SILEX_TOKEN_PUNCTUATOR);

				switch (lex.token.punctuator) {
					case '(': {
						SI_ASSERT(curFunc == nil && commaMode == false);

						scFunction* func;
						scInfoTable* funcScope;

						u32 params[128];
						usize paramsLen = 0;

						b32 res, funcIsNew;
						/* TODO(EimaMei): Atpažinti K&R apibrėžtis ir nepa-
						 * vadintas deklaracijas. */
						siHashEntry* entry;

						funcScope = si_mallocItem(alloc[SC_SCOPE], scInfoTable);
						*funcScope = *scope;
						funcScope->parent = scope;
						funcScope->rank = 1;

						entry = si_hashtableSetWithHash(global_scope.identifiers, name, nil, &funcIsNew);
						if (funcIsNew) {
							scIdentifierKey* key = si_malloc(alloc[SC_MAIN], sizeof(scIdentifierKey) + sizeof(scFunction));
							key->type = SC_IDENTIFIER_KEY_FUNC;
							/* key->rank = scope->rank; */

							func = (scFunction*)key->identifier;
							func->type = type;
							func->name = name;
							func->code = si_arrayMakeReserve(alloc[SC_MAIN], sizeof(scAction), 0);

							entry->value = key;
							global_scope.funcsLen += 1;
						}
						else {
							scIdentifierKey* key = entry->value;
							SI_ASSERT(key->type == SC_IDENTIFIER_KEY_FUNC);
							func = (scFunction*)key->identifier;
						}

						paramLoop: {
							res = silex_lexerTokenGet(&lex);

							scType type = sc_typeGetAndMake(&lex, funcScope);
							SI_ASSERT(res && lex.type == SILEX_TOKEN_IDENTIFIER);
							u64 hash = lex.token.identifier.hash;

							if (!funcIsNew) {
								scType* oldType = &func->paramTypes[paramsLen];
								SI_ASSERT(sc_typeCmp(&type, oldType));
							}
							else {
								scIdentifierKey* key = si_malloc(alloc[SC_MAIN], sizeof(scIdentifierKey) + sizeof(scVariable));
								key->type = SC_IDENTIFIER_KEY_VAR;
								key->rank = funcScope->rank;

								scVariable* pVar = (scVariable*)key->identifier;
								pVar->type = type;
								pVar->init = nil;

								siHashEntry* param = si_hashtableSetWithHash(funcScope->identifiers, hash, key, &res);
								SI_ASSERT(res);

								params[paramsLen] = param - funcScope->identifiers;
								paramsLen += 1;
							}

							do {
								res = silex_lexerTokenGet(&lex);
								SI_ASSERT(res);
							} while (lex.type != SILEX_TOKEN_PUNCTUATOR);
							scPunctuator punc = lex.token.punctuator;

							switch (punc) {
								case ',': goto paramLoop;
								case ')': break;
								default: SI_PANIC();
							}
						}

						res = silex_lexerTokenGet(&lex);
						SI_ASSERT(res && lex.type == SILEX_TOKEN_PUNCTUATOR);

						if (funcIsNew) {
							func->paramLen = paramsLen;
							func->paramVars = si_mallocArray(alloc[SC_MAIN], u32, paramsLen);
							func->paramTypes = si_mallocArray(alloc[SC_MAIN], scType, paramsLen);
						}

						switch (lex.token.punctuator) {
							case '{': {
								curFunc = func;
								scope = funcScope;

								for_range (i, 0, paramsLen * funcIsNew) {
									usize index = params[i];
									scIdentifierKey* key = funcScope->identifiers[index].value;

									func->paramTypes[i] = ((scVariable*)key->identifier)->type;
									func->paramVars[i] = index;
									funcScope->varsLen += 1;
								}

								break;
							}
							case ';': {
								for_range (i, 0, paramsLen) {
									usize index = params[i];
									scIdentifierKey* key = funcScope->identifiers[index].value;

									func->paramTypes[i] = ((scVariable*)key->identifier)->type;
									func->paramVars[i] = index;
									key->rank = UINT16_MAX;
								}
								break;
							}
							default: SI_PANIC_MSG("Cannot use this punctuator after a function declaration");
						}

						if (name == hash_main) {
							sc_functionValidateMain(func);
							global_scope.mainFuncID = func - global_scope.funcs;
							hash_main = 0;
						}
						break;
					}

					case '=': {
						scIdentifierKey* key = sc_identifierKeyGet(scope, name, SC_IDENTIFIER_KEY_VAR);
						scVariable* pVar = (scVariable*)key->identifier;
						pVar->type = type;

						action.type = SC_ACTION_VAR_ASSIGN;
						action.values = si_arrayMakeReserve(alloc[SC_MAIN], sizeof(scTokenStruct), 2);
						si_arrayPush(&action.values, key);

						scPunctuator punc = sc_actionAddValues(&lex, &action);
						if (curFunc == nil) {
							SI_PANIC();
						}

						si_arrayPush(&curFunc->code, action);
						scope->varsLen += 1;

						switch (punc) {
							case ',':
								if (type.ptr) {
									type = *type.ptr;
								}
								commaMode = true;
								res = silex_lexerTokenGet(&lex);
								goto type_section_start;
							case ';': break;
							default: SI_PANIC_MSG("Expression should end with a semicolin or continued via a comma punctuator.");
						}

						break;
					}

					case ';':
						si_printf("Found a variable that exists.\n");
						break;
					default: si_printf("%c\n", lex.token.punctuator); SI_PANIC();
				}
				continue;

keyword_section:
				switch (lex.token.keyword) {
					case SILEX_KEYWORD_RETURN: {
						scAction action;
						action.type = SC_ACTION_RETURN;
						action.values = si_arrayMakeReserve(alloc[SC_MAIN], sizeof(scTokenStruct), 1);

						sc_actionAddValues(&lex, &action);
						si_arrayPush(&curFunc->code, action);
						break;
					}
					case SILEX_KEYWORD_TYPEDEF: {
						scType type;
						u64 name;
						res = silex_lexerTokenGet(&lex);
						SI_ASSERT(res);

						type = sc_typeGetAndMake(&lex, scope);
						SI_ASSERT(type.size != -2);
						SI_ASSERT(lex.type == SILEX_TOKEN_IDENTIFIER);
						name = lex.token.identifier.hash;

						scIdentifierKey* key = sc_identifierKeyGet(scope, name, SC_IDENTIFIER_KEY_TYPE);
						scType* typedefType = (scType*)key->identifier;
						*typedefType = type;

						res = silex_lexerTokenGet(&lex);
						SI_ASSERT(res && lex.type == SILEX_TOKEN_PUNCTUATOR && lex.token.punctuator == ';');

						break;
					}
					default: SI_PANIC();
				}

				break;
			}

			case SILEX_TOKEN_PUNCTUATOR: {
				switch (lex.token.punctuator) {
					case '{': {
						SI_PANIC();
						scAction action;
						action.type = SC_AST_SCOPE_BEGIN;
						action.values = (scTokenStruct*)scope;
						si_arrayPush(&curFunc->code, action);
						break;
					}
					case '}': {
						scInfoTable* oldScope = scope;
						scope = scope->parent;
						SI_ASSERT_NOT_NULL(scope);

						usize dif = (oldScope->varsLen + oldScope->typesLen) - (scope->varsLen + scope->typesLen);
						SI_ARRAY_HEADER(scope->identifiers)->len -= dif;
						si_allocatorResetSub(alloc[SC_SCOPE], sizeof(scInfoTable));

						if (scope->parent == nil) {
							sc_parseFunction(oldScope, curFunc, asm);
							curFunc = nil;
							break;
						}

						SI_PANIC();
						scAction action;
						action.type = SC_AST_SCOPE_END;
						action.values = (scTokenStruct*)scope;
						si_arrayPush(&curFunc->code, action);
						break;
					}
				}
				break;
			}
		}
	}
	si_timeStampPrintSince(ts);
#endif
	SI_ASSERT_MSG(global_scope.mainFuncID != 0, "no main, no game");



	SC_ALLOCATOR_MAKE(
		SC_X86ASM,
		USIZE_MAX,
		16 * sizeof(u8) * si_arrayLen(asm)
	);


	x86EnvironmentState x86;
	x86.data = si_mallocArray(alloc[SC_X86ASM], u8, alloc[SC_X86ASM]->maxLen);
	x86.len = 0;
	x86.conv = X86_CALLING_CONV_SYSTEM_V_X86;
	x86.registers = 0;

	ts = si_timeStampStart();

	for_range (i, 0, si_arrayLen(asm)) {
		scAsm* instruction = &asm[i];

		switch (instruction->type) {
			case SC_ASM_FUNC_START: {
				scFunction* func = &global_scope.funcs[instruction->src];
				func->location = x86.len;
				x86.registers = 0;
				break;
			}

			case SC_ASM_PUSH_R64: {
				sc_x86OpcodePush(&x86, X86_PUSH_R64 + RBP);

				sc_x86Opcode(
					&x86, X86_MOV_RM32_R32, RBP, RSP,
					X86_CFG_REX_PREFIX | X86_CFG_RMB | X86_CFG_64BIT | X86_CFG_DST_R | X86_CFG_SRC_R
				);
				break;
			}

			case SC_ASM_LD_M32_PARAM: {
				x86Register reg = sc_x86PickFunctionArg(&x86);
				sc_x86Opcode(
					&x86, X86_MOV_RM32_R32, instruction->dst, reg,
					X86_CFG_RMB | X86_CFG_DST_M | X86_CFG_SRC_R
				);
				break;
			}

			case SC_ASM_LD_M64_PARAM: {
				x86Register reg = sc_x86PickFunctionArg(&x86);
				sc_x86Opcode(
					&x86, X86_MOV_RM32_R32, instruction->dst, reg,
					X86_CFG_RMB | X86_CFG_64BIT | X86_CFG_DST_M | X86_CFG_SRC_R
				);
				break;
			}

			case SC_ASM_LD_R32_I32: {
				sc_x86Opcode(
					&x86, X86_MOV_RM32_I32, instruction->dst, instruction->src,
					X86_CFG_RMB | X86_CFG_ID | X86_CFG_DST_R
				);
				break;
			}

			X86_ASM_TEMPLATE_R_M(&x86, SC_ASM_LD_R8_M8, X86_MOV_R8_RM8, instruction)
			X86_ASM_TEMPLATE_M_I(&x86, SC_ASM_LD_M8_I8, X86_MOV_RM8_I8, instruction)

			case SC_ASM_LD_M32_M32:
				x86_OP_M32_M32(&x86, instruction, X86_MOV_RM32_R32);
				break;

			case SC_ASM_ADD_R32_I32: {
				x86Register reg = sc_x86RegisterConvert(&x86, instruction->dst);
				sc_x86Opcode(
					&x86, X86_ADD_RM32_I32, reg, instruction->src,
					X86_CFG_RMB | X86_CFG_DST_R | X86_CFG_ID | X86_CFG_ADD
				);
				break;
			}

			case SC_ASM_SUB_R32_I32: {
				x86Register reg = sc_x86RegisterConvert(&x86, instruction->dst);
				sc_x86Opcode(
					&x86, X86_SUB_RM32_I32, reg, instruction->src,
					X86_CFG_RMB | X86_CFG_DST_R | X86_CFG_ID | X86_CFG_SUB
				);
				break;
			}

			case SC_ASM_ADD_R32_M32: {
				x86Register reg = sc_x86RegisterConvert(&x86, instruction->dst);
				sc_x86Opcode(
					&x86, X86_ADD_R32_RM32, reg, instruction->src,
					X86_CFG_RMB | X86_CFG_DST_R | X86_CFG_SRC_M
				);
				break;
			}

			case SC_ASM_SUB_R32_M32: {
				x86Register reg = sc_x86RegisterConvert(&x86, instruction->dst);
				sc_x86Opcode(
					&x86, X86_SUB_R32_RM32, reg, instruction->src,
					X86_CFG_RMB | X86_CFG_DST_R | X86_CFG_SRC_M
				);
				break;
			}

			case SC_ASM_ADD_M32_I32: {
				sc_x86Opcode(
					&x86, X86_ADD_RM32_I32, instruction->dst, instruction->src,
					X86_CFG_RMB | X86_CFG_DST_M | X86_CFG_ID | X86_CFG_ADD
				);
				break;
			}
			case SC_ASM_ADD_M32_M32:
				x86_OP_M32_M32_EX(&x86, instruction, X86_ADD_RM32_R32, X86_CFG_ADD);
				break;

			case SC_ASM_SUB_M32_I32: {
				sc_x86Opcode(
					&x86, X86_SUB_RM32_I32, instruction->dst, instruction->src,
					X86_CFG_RMB | X86_CFG_DST_M | X86_CFG_ID | X86_CFG_SUB
				);
				break;
			}
			case SC_ASM_SUB_M32_M32:
				x86_OP_M32_M32_EX(&x86, instruction, X86_SUB_RM32_R32, X86_CFG_SUB);
				break;


			case SC_ASM_RET_R32: {
				x86Register reg = sc_x86RegisterConvert(&x86, instruction->src);

				if (reg != RAX) {
					sc_x86Opcode(
						&x86, X86_MOV_R32_I32, RAX, reg,
						X86_CFG_ID | X86_CFG_DST_R
					);
				}
				sc_x86OpcodePush(&x86, X86_POP_R64 + RBP);
				sc_x86OpcodePush(&x86, X86_RET);

				break;
			}

			case SC_ASM_RET_I32: {
				sc_x86Opcode(
					&x86, X86_MOV_R32_I32, RAX, instruction->src,
					X86_CFG_ID | X86_CFG_DST_R
				);
				sc_x86OpcodePush(&x86, X86_POP_R64 + RBP);
				sc_x86OpcodePush(&x86, X86_RET);

				break;
			}

			case SC_ASM_RET_M8: case SC_ASM_RET_M16:
			case SC_ASM_RET_M32: {
				sc_x86Opcode(
					&x86, X86_MOV_R32_RM32, RAX, instruction->src,
					X86_CFG_RMB | X86_CFG_DST_R | X86_CFG_SRC_M
				);
				sc_x86OpcodePush(&x86, X86_POP_R64 + RBP);
				sc_x86OpcodePush(&x86, X86_RET);

				break;
			}
			default: si_printf("%i\n", instruction->type); SI_PANIC();
		}
	}

	si_timeStampPrintSince(ts);

	usize _startFuncStart = x86.len;
	scAsmType _x86_64StartFunc[] = {
		SC_ASM_LD_R64_M64, /* mov <param0>, [RSP] */
		SC_ASM_ARITH_R64_M64, /* lea <param1>, [RSP + 4] */
		SC_ASM_CALL,
		SC_ASM_SYSCALL
	};

	scAsmType* _startFunc = _x86_64StartFunc;
	usize _startFuncLen = countof(_x86_64StartFunc);
	x86.registers = 0;

	ts = si_timeStampStart();

	for_range (i, 0, _startFuncLen) {
		switch (_startFunc[i]) {
			case SC_ASM_LD_R64_M64: {
				x86Register reg = sc_x86PickFunctionArg(&x86);
				sc_x86OpcodeEx(
					&x86, X86_MOV_R32_RM32, reg, 0, RSP,
					X86_CFG_RMB | X86_CFG_64BIT | X86_CFG_SIB
				);

				break;
			}
			case SC_ASM_ARITH_R64_M64: {
				x86Register reg = sc_x86PickFunctionArg(&x86);
				sc_x86OpcodeEx(
					&x86, X86_LEA_R32_RM32, reg, 4, RSP,
					X86_CFG_RMB | X86_CFG_64BIT | X86_CFG_SIB | X86_CFG_SRC_M | X86_CFG_SRC_M_NOT_NEG
				);

				break;
			}
			case SC_ASM_CALL: {
				scFunction* mainFunc = &global_scope.funcs[global_scope.mainFuncID];
				i32 adr = mainFunc->location - x86.len - (sizeof(u8) + sizeof(u32));
				sc_x86Opcode(
					&x86, X86_CALL_REL32, 0, adr,
					X86_CFG_ID
				);

				break;
			}
			case SC_ASM_SYSCALL: {
				sc_x86Opcode(
				   	&x86, X86_MOV_RM32_R32, EDI, EAX,
				   	X86_CFG_RMB | X86_CFG_DST_R | X86_CFG_SRC_R
				);
				sc_x86Opcode(
					&x86, X86_MOV_RM32_I32, RAX, 60,
					X86_CFG_RMB | X86_CFG_ID | X86_CFG_DST_R | X86_CFG_64BIT
				);
				sc_x86OpcodePush2(&x86, X86_SYSCALL);
				break;
			}

			default: SI_PANIC();
		}
	}

	si_timeStampPrintSince(ts);

	ts = si_timeStampStart();

	usize elfSize = sizeof(elf64ElfHeader) + sizeof(elf64ProgramHeader);
	usize fileLen = elfSize + x86.len;

	alloc[SC_EXE] = si_allocatorMake(fileLen);
	u8* buf = si_mallocArray(alloc[SC_EXE], u8, fileLen);

	elf64_elfHeaderMake((elf64ElfHeader*)&buf[0], true, ELFOSABI_NONE, EM_X86_64, _startFuncStart);
	elf64_programHeaderMake((elf64ProgramHeader*)&buf[sizeof(elf64ElfHeader)], fileLen);
	memcpy(&buf[elfSize], x86.data, x86.len);

	siFile exe = si_fileCreate("a.out");
	si_fileWriteLen(&exe, buf, fileLen);
	si_fileClose(exe);

	si_timeStampPrintSince(ts);

	for_range (i, 0, countof(alloc)) {
		si_allocatorFree(alloc[i]);
	}
}
