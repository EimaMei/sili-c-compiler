#define SILEX_USE_HASH
#include <sili.h>
#include <sililex.h>

#include <scc.h>
#include <x86.h>
#include <exegen.h>


scType type_char     = (scType){1, SC_TYPE_INT, 0, nil};
scType type_short    = (scType){2, SC_TYPE_INT, 0, nil};
scType type_int      = (scType){4, SC_TYPE_INT, 0, nil};
scType type_long     = (scType){8, SC_TYPE_INT, 0, nil};
scType type_unsigned = (scType){4, SC_TYPE_INT | SC_TYPE_UNSIGNED, 0, nil};
scType type_float    = (scType){4, SC_TYPE_FLOAT, 0, nil};
scType type_double   = (scType){8, SC_TYPE_FLOAT, 0, nil};

scType type_size_t   = (scType){8, SC_TYPE_INT | SC_TYPE_UNSIGNED, 0, nil};

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



void sc_astNodePrint(const scAstNode* node, u32 depth, b32 isRight, b32 showLetter, u32* rootLevel) {
	if (node == nil) return;

    if (depth > 0) {
        for_range(i, 0, depth - 1) { si_print("  "); }
		char letter = isRight ? 'R' : 'L';
        si_printf("%s──%c%i ", isRight ? "└" : "├", showLetter ? letter : '\0', *rootLevel);
    }

    switch (node->type) {
        case SC_AST_NODE_TYPE_IDENTIFIER:
            SI_LOG_FMT("%*s\n", node->data.identifier.len, node->data.identifier.text);
            break;

        case SC_AST_NODE_TYPE_CONSTANT:
            switch (node->data.constant.type) {
                case SILEX_CONSTANT_NUM_SIGNED:
                    SI_LOG_FMT("%lli\n", node->data.constant.value.integer);
                    break;
                case SILEX_CONSTANT_NUM_UNSIGNED:
                    SI_LOG_FMT("%llu\n", node->data.constant.value.integer);
                    break;
                case SILEX_CONSTANT_FLOAT:
                    SI_LOG_FMT("%f\n", node->data.constant.value.floats);
                    break;
            }
            break;

        case SC_AST_NODE_TYPE_BINARY_OP:
			*rootLevel += 1;
			u32 og = *rootLevel;
            SI_LOG_FMT("%s\n", silex_operatorCstr(node->data.binary.operator));
            sc_astNodePrint(node->data.binary.left, depth + 1, false, true, rootLevel);
            sc_astNodePrint(node->data.binary.right, depth + 1, true, true, &og);
            break;

        case SC_AST_NODE_TYPE_UNARY_OP:
            SI_LOG_FMT("%s\n", silex_operatorCstr(node->data.unary.operator));
            sc_astNodePrint(node->data.unary.operand, depth + 1, true, false, rootLevel);
            break;

		case SC_AST_NODE_TYPE_GROUP_OP:
            SI_LOG("()\n");
			sc_astNodePrint(node->data.group.start, depth + 1, true, false, rootLevel);
			break;
    }
}

void sc_astNodeToAsm(scAsmEnvironmentState* state, scAsm* instructions, scOperator assignment,
		scAsmType asmTypes[3], u32 typeSize, scAction* action, scAstNode* node,
		scIdentifierKey* key, scAsmRegister regSrc) {
	scAsm asm;

	switch (node->type) {
		case SC_AST_NODE_TYPE_CONSTANT: {
			scConstant constant = node->data.constant;

			/* TODO(EimaMei): SC_ASM_LD_M64_I64. */
			asm.type = sc_asmGetCorrectType(asmTypes[0], typeSize);
			asm.src = constant.value.integer;

			if (key) {
				SI_ASSERT(key->type == SC_IDENTIFIER_KEY_VAR);
				scVariable* var = (scVariable*)key->identifier;
				asm.dst = var->location;
			}
			else {
				asm.dst = regSrc;
			}

			si_arrayPush(&instructions, asm);
			break;
		}

		case SC_AST_NODE_TYPE_IDENTIFIER: {
			u64 hash = node->data.identifier.hash;
			scIdentifierKey* srcKey = si_hashtableGetWithHash(state->scope->identifiers, hash);
			SI_ASSERT(srcKey->type != SC_IDENTIFIER_KEY_FUNC);

			scVariable* src = (scVariable*)srcKey->identifier;

			asm.type = sc_asmGetCorrectType(asmTypes[1], src->type.size);
			asm.src = src->location;

			if (key) {
				SI_ASSERT(key->type == SC_IDENTIFIER_KEY_VAR);
				scVariable* dst = (scVariable*)key->identifier;
				asm.dst = dst->location;
			}
			else {
				asm.dst = regSrc;
			}
			si_printf("%i %p %i | %i\n", asm.dst, key, asm.type, asmTypes[2]);

			si_arrayPush(&instructions, asm);
			break;
		}

		case SC_AST_NODE_TYPE_UNARY_OP: {
			scAsmType type;
			switch (node->data.unary.operator) {
				case SILEX_OPERATOR_MINUS: type = SC_ASM_NEG_R8; break;
				case SILEX_OPERATOR_TILDE: type = SC_ASM_NOT_R8; break;
				default: SI_PANIC();
			}
			node = node->data.unary.operand;

			if (asmTypes[2] == 0 || node->type == SC_AST_NODE_TYPE_UNARY_OP) {
				sc_astNodeToAsm(state, instructions, assignment, asmTypes, typeSize, action, node, key, regSrc);

				asm.type = sc_asmGetCorrectType(type, typeSize);
				asm.dst = ((scAsm*)si_arrayBack(instructions))->dst;
				asm.src = 0;
				si_arrayPush(&instructions, asm);

				break;
			}

			u64 hash = node->data.identifier.hash;
			scIdentifierKey* srcKey = si_hashtableGetWithHash(state->scope->identifiers, hash);
			SI_ASSERT(srcKey->type != SC_IDENTIFIER_KEY_FUNC);

			scVariable* src = (scVariable*)srcKey->identifier;

			asm.type = sc_asmGetCorrectType(SC_ASM_LD_R8_M8, typeSize);
			asm.dst = sc_asmRegisterAny(state, false);
			asm.src = src->location;
			si_arrayPush(&instructions, asm);

			asm.type = sc_asmGetCorrectType(type, typeSize);
			si_arrayPush(&instructions, asm);

			asm.type = sc_asmGetCorrectType(asmTypes[2], typeSize);

			if (key != nil) {
				SI_ASSERT(key->type == SC_IDENTIFIER_KEY_VAR);
				scVariable* dst = (scVariable*)key->identifier;
				asm.src = dst->location;
			}
			else {
				asm.src = regSrc;
			}

			si_arrayPush(&instructions, asm);
			break;
		}

		case SC_AST_NODE_TYPE_GROUP_OP: {
			if (asmTypes[2] == 0) {
				sc_astNodeToAsm(state, instructions, assignment, asmTypes, typeSize, action, node->data.group.start, key, regSrc);
				break;
			}

			u32 oldAsmType = asmTypes[2];
			asmTypes[0] = SC_ASM_LD_R8_I8; // : sc_asmGetCorrectOperator(SC_ASM_ADD_R8_I8, assignment);
			asmTypes[1] = SC_ASM_LD_R8_M8; // : sc_asmGetCorrectOperator(SC_ASM_ADD_R8_M8, assignment);
			asmTypes[2] = 0;

			scAsmType regSrcNew = sc_asmRegisterAny(state, true);
			asm.type = SC_ASM_REG_SET;
			asm.dst = regSrcNew;
			si_arrayPush(&instructions, asm);

			sc_astNodeToAsm(state, instructions, 0, asmTypes, typeSize, action, node->data.group.start, nil, regSrcNew);

			asm.type = sc_asmGetCorrectType(oldAsmType, typeSize);
			asm.src = regSrcNew;

			if (key != nil) {
				asm.dst = ((scVariable*)key->identifier)->location;
			}
			else {
				asm.dst = regSrc;
			}

			si_arrayPush(&instructions, asm);

			sc_asmRegisterUnset(state, regSrcNew);
			asm.type = SC_ASM_REG_UNSET;
			asm.dst = regSrcNew;
			si_arrayPush(&instructions, asm);
			break;
		}


		case SC_AST_NODE_TYPE_BINARY_OP: {
			scOperator operator = node->data.binary.operator;

			scAsmType regSrcNew = sc_asmRegisterAny(state, true);
			asm.type = SC_ASM_REG_SET;
			asm.dst = regSrcNew;
			si_arrayPush(&instructions, asm);

			asmTypes[0] = SC_ASM_LD_R8_I8;
			asmTypes[1] = SC_ASM_LD_R8_M8;
			asmTypes[2] = SC_ASM_LD_R8_R8;
			sc_astNodeToAsm(state, instructions, 0, asmTypes, typeSize, action, node->data.binary.left, nil, regSrcNew);

			asmTypes[0] = sc_asmGetCorrectOperator(SC_ASM_ADD_R8_I8, operator);
			asmTypes[1] = sc_asmGetCorrectOperator(SC_ASM_ADD_R8_M8, operator);
			asmTypes[2] = sc_asmGetCorrectOperator(SC_ASM_ADD_R8_R8, operator);
			sc_astNodeToAsm(state, instructions, 0, asmTypes, typeSize, action, node->data.binary.right, nil, regSrcNew);


			asm.src = regSrcNew;

			if (key != nil) {
				asm.type = sc_asmGetCorrectType(!assignment ? SC_ASM_LD_M8_R8 : sc_asmGetCorrectOperator(SC_ASM_ADD_M8_R8, assignment), typeSize);
				asm.dst = ((scVariable*)key->identifier)->location;
			}
			else {
				/* TODO(EimaMei): Why does THIS work? It has to be a bug... */
				asm.type = sc_asmGetCorrectType(!assignment ? SC_ASM_LD_R8_R8 : sc_asmGetCorrectOperator(SC_ASM_ADD_R8_R8, assignment), typeSize);
				asm.dst = regSrc;
			}

			si_arrayPush(&instructions, asm);

			sc_asmRegisterUnset(state, regSrcNew);
			asm.type = SC_ASM_REG_UNSET;
			asm.dst = regSrcNew;
			si_arrayPush(&instructions, asm);

			break;
		}
		default: SI_PANIC();
	}
}

void sc_astNodeOptimize(scAstNode* node) {
	SI_STOPIF(node == nil, return);
	SI_STOPIF(node->type != SC_AST_NODE_TYPE_BINARY_OP, return);

	scAstNode* args[] = {node->data.binary.left, node->data.binary.right};
	for_range (i, 0, countof(args)) {
		scAstNode* arg = args[i];

		switch (arg->type) {
			case SC_AST_NODE_TYPE_BINARY_OP:
				sc_astNodeOptimize(arg);
				break;
			case SC_AST_NODE_TYPE_UNARY_OP: {
				scOperator operator = arg->data.unary.operator;
#if 0
				if (operator == SILEX_OPERATOR_TILDE) {
					operator = SILEX_OPERATOR_MINUS;

					scAstNode* other = args[i ^ 1];
					sc_astwhater(other);
				}

				if (node->data.binary.operator == operator) {
					node->data.binary.operator = SILEX_OPERATOR_PLUS;
					*arg = *arg->data.unary.operand;
				}
				else {
					node->data.binary.operator = SILEX_OPERATOR_MINUS;
					*arg = *arg->data.unary.operand;
				}
#endif
				break;
			}
			case SC_AST_NODE_TYPE_CONSTANT: {
				scAstNode* other = args[i ^ 1];
				if (other->type == SC_AST_NODE_TYPE_CONSTANT) {
					scOperator operator = node->data.binary.operator;
					sc_constantArithmetic(
						&arg[0].data.constant, operator, args[1]->data.constant
					);

					node->type = SC_AST_NODE_TYPE_CONSTANT;
					node->data.constant = args[0]->data.constant;
					i += 1;
				}
				break;
			}
		}
	}
}

void sc_parseFunction(scInfoTable* scope, scFunction* func, scAsm* instructions) {
	SI_LOG("== Parsing the function ==\n");

	SI_LOG_FMT("\tsi_arrayLen(func->code) = %i\n", si_arrayLen(func->code));
	for_range (i, 0, si_arrayLen(func->code)) {
		scAction* action = &func->code[i];

		switch (action->type) {
			case SC_ACTION_VAR_ADD:
			case SC_ACTION_VAR_SUB:
			case SC_ACTION_VAR_ASSIGN: {
				sc_astNodeMake(action, true);
				break;
			}

			case SC_ACTION_RETURN: {
				sc_astNodeMake(action, false);
				break;
			}
			default: SI_PANIC();
		}
	}
	SI_LOG("== AST nodes have been completed  ==\n");

	for_range (i, 0, si_arrayLen(func->code)) {
		scAction* action = &func->code[i];
		sc_astNodeOptimize(action->root);
	}
	SI_LOG("== AST node optimizations have been implemented ==\n");

	scAsmEnvironmentState state;
	state.scope = scope;
	state.registers = 0;

	scAsm asm;
	asm.type = SC_ASM_FUNC_START;
	asm.dst = func - global_scope.funcs;
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

		asm.type = sc_asmGetCorrectType(SC_ASM_LD_M8_R8, var->type.size);
		asm.dst = stack;
		asm.src = SC_ASM_REG_PARAM_0 + i;

		si_arrayPush(&instructions, asm);
	}
	SI_LOG("== Parameters -> scAsm complete  ==\n");

	for_range (i, 0, si_arrayLen(func->code)) {
		scAction* action = &func->code[i];
		SI_ASSERT_NOT_NULL(action);


		u32 level = 0;
		sc_astNodePrint(action->root, 4, false, false, &level);

		switch (action->type) {
			case SC_ACTION_VAR_ASSIGN: {
				scIdentifierKey* key = sc_actionIdentifierGet(action->values);
				scVariable* var = (scVariable*)key->identifier;
				stack = si_alignCeilEx(stack + var->type.size, var->type.size);
				var->location = stack;
				SI_STOPIF(si_arrayLen(action->values) == 1, break);

				sc_astNodeToAsm(
					&state, instructions, 0,
					si_buf(u32, SC_ASM_LD_M8_I8, SC_ASM_LD_M8_M8, 0),
					var->type.size, action, action->root, key,
					0
				);
				break;
			}
			case SC_ACTION_VAR_ADD: {
				scIdentifierKey* key = sc_actionIdentifierGet(action->values);
				scVariable* var = (scVariable*)key->identifier;

				sc_astNodeToAsm(
					&state, instructions, SILEX_OPERATOR_PLUS,
					si_buf(u32, SC_ASM_ADD_M8_I8, SC_ASM_ADD_M8_M8, 0),
					var->type.size, action, action->root, key,
					0
				);
				break;
			}
			case SC_ACTION_VAR_SUB: {
				scIdentifierKey* key = sc_actionIdentifierGet(action->values);
				scVariable* var = (scVariable*)key->identifier;


				sc_astNodeToAsm(
					&state, instructions, SILEX_OPERATOR_MINUS,
					si_buf(u32, SC_ASM_SUB_M8_I8, SC_ASM_SUB_M8_M8, 0),
					var->type.size, action, action->root, key,
					0
				);
				break;
			}
			case SC_ACTION_RETURN: {
				sc_astNodeToAsm(
					&state, instructions, 0,
					si_buf(u32, SC_ASM_LD_R8_I8, SC_ASM_LD_R8_M8, 0),
					func->type.size, action, action->root, nil,
					SC_ASM_REG_RET
				);

				asm.type = SC_ASM_RET;
				si_arrayPush(&instructions, asm);
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


void sc_actionMakePlusPlus(scActionType actionType,  scLexer* lex, scIdentifierKey* key, scFunction* curFunc) {
	scAction action;
	action.type = actionType;
	action.values = si_arrayMakeReserve(alloc[SC_MAIN], sizeof(scTokenStruct), 2);
	si_arrayPush(&action.values, key);

	scTokenStruct* t = &action.values[1];
	t->type = SILEX_TOKEN_CONSTANT;
	t->token.constant.type = SILEX_CONSTANT_NUM_SIGNED;
	t->token.constant.value.integer = 1;
	SI_ARRAY_HEADER(action.values)->len = 2;

	sc_tokenGet(lex);
	SI_ASSERT(lex->type == SILEX_TOKEN_PUNCTUATOR);
	si_arrayPush(&curFunc->code, action);

	switch (lex->token.punctuator) {
		case ',': case ';': break;
		default: SI_PANIC_MSG("Expression should end with a semicolin or continued via a comma punctuator.");
	}
}


int main(void) {
#if 0
	cstring keywords[] = {
		"auto", "break", "case", "char", "const", "continue", "default", "do",
		"double", "else", "enum", "extern", "float", "for", "goto", "if", "int", "long",
		"register", "return", "short", "signed", "sizeof", "static", "struct", "switch",
		"typedef", "union", "unsigned", "void", "volatile", "while",

		"__typeof", "__"
	};
	u64 keywords_U64[countof(keywords)];
	memset(keywords_U64, 0, sizeof(keywords_U64));

	for_range (i, 0, countof(keywords)) {
		usize len = si_cstrLen(keywords[i]);
		SI_ASSERT(len <= 8);
#if 1
		memcpy(&keywords_U64[i], keywords[i], len);

		char m[1024];
		memcpy(m, keywords[i], len + 1);
		si_cstrUpper(m);
		SI_LOG_FMT("SILEX_KEYWORD_%s, %#llX\n", m, keywords_U64[i]);
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
		SI_MEGA(2),
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

	global_scope.fileName = "res/simple.c";
	global_scope.funcName.text = nil;

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

	SI_LOG_FMT("%ll#X: %p %ll#X\n", (n) ? *n : 0, n, hash);
	SI_PANIC();
#endif

#if 1
	siTimeStamp ts = si_timeStampStart();
	scLexer lex = silex_lexerMake(text, textLen);
	scType* baseType;
	b32 commaMode = false;
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
						sc_tokenGet(&lex);

						switch (lex.type) {
							case SILEX_TOKEN_OPERATOR: {
								switch (lex.token.operator) {
									case SILEX_OPERATOR_PLUS_PLUS:
										sc_actionMakePlusPlus(SC_ACTION_VAR_ADD, &lex, key, curFunc);
										break;
									case SILEX_OPERATOR_MINUS_MINUS:
										sc_actionMakePlusPlus(SC_ACTION_VAR_SUB, &lex, key, curFunc);
										break;
									case SILEX_OPERATOR_PLUS_ASSIGN: {
										action.type = SC_ACTION_VAR_ADD;
										action.values = si_arrayMakeReserve(alloc[SC_MAIN], sizeof(scTokenStruct), 2);
										si_arrayPush(&action.values, key);

										scPunctuator punc = sc_actionAddValues(&lex, scope, &action);
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

										scPunctuator punc = sc_actionAddValues(&lex, scope, &action);
										si_arrayPush(&curFunc->code, action);

										switch (punc) {
											case ',': case ';': break;
											default: SI_PANIC_MSG("Expression should end with a semicolin or continued via a comma punctuator.");
										}
										break;
									}

									default: continue;
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
				scString name = lex.token.identifier;

				sc_tokenGet(&lex);
				SI_ASSERT(lex.type == SILEX_TOKEN_PUNCTUATOR);

				switch (lex.token.punctuator) {
					case '(': {
						SI_ASSERT(curFunc == nil && commaMode == false);

						scFunction* func;
						scInfoTable* funcScope;

						u32 params[128];
						usize paramsLen = 0;

						b32 funcIsNew;
						/* TODO(EimaMei): Atpažinti K&R apibrėžtis ir nepa-
						 * vadintas deklaracijas. */
						siHashEntry* entry;

						funcScope = si_mallocItem(alloc[SC_SCOPE], scInfoTable);
						*funcScope = *scope;
						funcScope->parent = scope;
						funcScope->rank = 1;

						entry = si_hashtableSetWithHash(global_scope.identifiers, name.hash, nil, &funcIsNew);
						if (funcIsNew) {
							scIdentifierKey* key = si_malloc(alloc[SC_MAIN], sizeof(scIdentifierKey) + sizeof(scFunction));
							key->type = SC_IDENTIFIER_KEY_FUNC;
							/* key->rank = scope->rank; */

							func = (scFunction*)key->identifier;
							func->type = type;
							func->name = name.hash;
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
							sc_tokenGet(&lex);

							scType type = sc_typeGetAndMake(&lex, funcScope);
							SI_ASSERT(lex.type == SILEX_TOKEN_IDENTIFIER);
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

								b32 res;
								siHashEntry* param = si_hashtableSetWithHash(funcScope->identifiers, hash, key, &res);
								SI_ASSERT(res);

								params[paramsLen] = param - funcScope->identifiers;
								paramsLen += 1;
							}

							do {
								sc_tokenGet(&lex);
							} while (lex.type != SILEX_TOKEN_PUNCTUATOR);
							scPunctuator punc = lex.token.punctuator;

							switch (punc) {
								case ',': goto paramLoop;
								case ')': break;
								default: SI_PANIC();
							}
						}

						sc_tokenGet(&lex);
						SI_ASSERT(lex.type == SILEX_TOKEN_PUNCTUATOR);

						if (funcIsNew) {
							func->paramLen = paramsLen;
							func->paramVars = si_mallocArray(alloc[SC_MAIN], u32, paramsLen);
							func->paramTypes = si_mallocArray(alloc[SC_MAIN], scType, paramsLen);
						}

						switch (lex.token.punctuator) {
							case '{': {
								curFunc = func;
								global_scope.funcName = name;
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

						if (name.hash == hash_main) {
							sc_functionValidateMain(func);
							global_scope.mainFuncID = func - global_scope.funcs;
							hash_main = 0;
						}
						break;
					}

					case ',':
					case ';':
					case '=': {
						scIdentifierKey* key = sc_identifierKeyGet(scope, name.hash, SC_IDENTIFIER_KEY_VAR);
						scVariable* pVar = (scVariable*)key->identifier;
						pVar->type = type;

						action.type = SC_ACTION_VAR_ASSIGN;
						action.values = si_arrayMakeReserve(alloc[SC_MAIN], sizeof(scTokenStruct), 2);
						si_arrayPush(&action.values, key);

						scPunctuator punc = (lex.token.punctuator == '=')
							? sc_actionAddValues(&lex, scope, &action)
							: lex.token.punctuator;

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
								sc_tokenGet(&lex);
								goto type_section_start;
							case ';': break;
							default: SI_LOG_FMT("%i\n", punc); SI_PANIC_MSG("Expression should end with a semicolin or continued via a comma punctuator.");
						}

						break;
					}

					default: SI_LOG_FMT("%c\n", lex.token.punctuator); SI_PANIC();
				}
				continue;

keyword_section:
				switch (lex.token.keyword) {
					case SILEX_KEYWORD_RETURN: {
						action.type = SC_ACTION_RETURN;
						action.values = si_arrayMakeReserve(alloc[SC_MAIN], sizeof(scTokenStruct), 1);

						sc_actionAddValues(&lex, scope, &action);
						si_arrayPush(&curFunc->code, action);
						break;
					}
					case SILEX_KEYWORD_TYPEDEF: {
						scType type;
						u64 name;
						sc_tokenGet(&lex);

						type = sc_typeGetAndMake(&lex, scope);
						SI_ASSERT(type.size != -2);
						SI_ASSERT(lex.type == SILEX_TOKEN_IDENTIFIER);
						name = lex.token.identifier.hash;

						scIdentifierKey* key = sc_identifierKeyGet(scope, name, SC_IDENTIFIER_KEY_TYPE);
						scType* typedefType = (scType*)key->identifier;
						*typedefType = type;

						sc_tokenGet(&lex);
						SI_ASSERT(lex.type == SILEX_TOKEN_PUNCTUATOR && lex.token.punctuator == ';');

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
						action.type = SC_ACTION_SCOPE_BEGIN;
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
							global_scope.funcName.text = nil;
							break;
						}

						SI_PANIC();
						scAction action;
						action.type = SC_ACTION_SCOPE_END;
						action.values = (scTokenStruct*)scope;
						si_arrayPush(&curFunc->code, action);
						break;
					}
				}
				break;
			}
			case SILEX_TOKEN_OPERATOR: {
				switch (lex.token.operator) {
					case SILEX_OPERATOR_PLUS_PLUS: {
						sc_tokenGet(&lex);
						SI_ASSERT_MSG(lex.type == SILEX_TOKEN_IDENTIFIER, "Expected an identifier");

						scString identifier = lex.token.identifier;
						scIdentifierKey* key = si_hashtableGetWithHash(scope->identifiers, identifier.hash);

						SI_ASSERT_FMT(
							key != nil && key->rank <= scope->rank,
							"Variable '%*s' doesn't exist", identifier.len, lex.curData - identifier.len
						);
						SI_ASSERT_FMT(
							key->type == SC_IDENTIFIER_KEY_VAR,
							"Identifier '%*s' is not a variable.", identifier.len, lex.curData - identifier.len
						);

						sc_actionMakePlusPlus(SC_ACTION_VAR_ADD, &lex, key, curFunc);
						break;
					}
					case SILEX_OPERATOR_MINUS_MINUS: {
						sc_tokenGet(&lex);
						SI_ASSERT_MSG(lex.type == SILEX_TOKEN_IDENTIFIER, "Expected an identifier");

						scString identifier = lex.token.identifier;
						scIdentifierKey* key = si_hashtableGetWithHash(scope->identifiers, identifier.hash);

						SI_ASSERT_FMT(
							key != nil && key->rank <= scope->rank,
							"Variable '%*s' doesn't exist", identifier.len, lex.curData - identifier.len
						);
						SI_ASSERT_FMT(
							key->type == SC_IDENTIFIER_KEY_VAR,
							"Identifier '%*s' is not a variable.", identifier.len, lex.curData - identifier.len
						);

						sc_actionMakePlusPlus(SC_ACTION_VAR_SUB, &lex, key, curFunc);
						break;
					}

					default: continue;
				}
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
	x86.root.registers = 0;
	x86.data = si_mallocArray(alloc[SC_X86ASM], u8, alloc[SC_X86ASM]->maxLen);
	x86.len = 0;
	x86.conv = X86_CALLING_CONV_SYSTEM_V_X86;

	ts = si_timeStampStart();

	for_range (i, 0, si_arrayLen(asm)) {
		scAsm* instruction = &asm[i];

		switch (instruction->type) {
			case SC_ASM_FUNC_START: {
				scFunction* func = &global_scope.funcs[instruction->dst];
				func->location = x86.len;
				x86.root.registers = 0;
				break;
			}
			case SC_ASM_REG_SET:
				sc_asmRegisterSet(&x86.root, instruction->dst);
				break;
			case SC_ASM_REG_UNSET:
				sc_asmRegisterUnset(&x86.root, instruction->dst);
				break;


			case SC_ASM_PUSH_R64: {
				sc_x86OpcodePush(&x86, X86_PUSH_R64 + RBP);

				sc_x86Opcode(
					&x86, X86_MOV_RM8_R8 + 1, RBP, RSP,
					X86_CFG_64BIT | X86_CFG_RMB | X86_CFG_DST_R | X86_CFG_SRC_R
				);
				break;
			}

			/* ====== SC_ASM_LD_<>_<> ======= */
			/* === SC_ASM_LD_R8_<> ==== */
			X86_ASM_TEMPLATE__REG_REG_RMB(&x86, SC_ASM_LD_R8_R8, X86_MOV_R8_RM8, instruction)
			X86_ASM_TEMPLATE__REG_MEM_RMB(&x86, SC_ASM_LD_R8_M8, X86_MOV_R8_RM8, instruction)
			X86_ASM_TEMPLATE__REG_RMB_INT(&x86, SC_ASM_LD_R8_I8, X86_MOV_RM8_I8, instruction)
			/* === SC_ASM_LD_M8_<> ==== */
			X86_ASM_TEMPLATE_RMB__MEM_REG(&x86, SC_ASM_LD_M8_R8, X86_MOV_RM8_R8, instruction)
			X86_ASM_TEMPLATE_RMB__MEM_MEM(&x86, SC_ASM_LD_M8_M8, X86_MOV_RM8_R8, instruction)
			X86_ASM_TEMPLATE_RMB__MEM_ID(&x86, SC_ASM_LD_M8_I8, X86_MOV_RM8_I8, instruction)


			/* ====== SC_ASM_ADD_<>_<> ======= */
			/* === SC_ASM_ADD_R8_<> ==== */
			X86_ASM_TEMPLATE__REG_REG_RMB(&x86, SC_ASM_ADD_R8_R8, X86_ADD_RM8_R8, instruction)
			X86_ASM_TEMPLATE__REG_MEM_RMB(&x86, SC_ASM_ADD_R8_M8, X86_ADD_R8_RM8, instruction)
			X86_ASM_TEMPLATE__REG_RMB_INT_EX(&x86, SC_ASM_ADD_R8_I8, X86_ADD_RM8_I8, instruction, X86_CFG_ADD)
			/* === SC_ASM_ADD_M8_<> ==== */
			X86_ASM_TEMPLATE_RMB__MEM_REG(&x86, SC_ASM_ADD_M8_R8, X86_ADD_RM8_R8, instruction)
			X86_ASM_TEMPLATE_RMB__MEM_MEM_EX(&x86, SC_ASM_ADD_M8_M8, X86_ADD_RM8_R8, instruction, X86_CFG_ADD)
			X86_ASM_TEMPLATE_RMB__MEM_ID_EX(&x86, SC_ASM_ADD_M8_I8, X86_ADD_RM8_I8, instruction, X86_CFG_ADD)


			/* ====== SC_ASM_SUB_<>_<> ======= */
			/* === SC_ASM_SUB_R8_<> ==== */
			X86_ASM_TEMPLATE__REG_REG_RMB(&x86, SC_ASM_SUB_R8_R8, X86_SUB_RM8_R8, instruction)
			X86_ASM_TEMPLATE__REG_MEM_RMB(&x86, SC_ASM_SUB_R8_M8, X86_SUB_R8_RM8, instruction)
			X86_ASM_TEMPLATE__REG_RMB_INT_EX(&x86, SC_ASM_SUB_R8_I8, X86_SUB_RM8_I8, instruction, X86_CFG_SUB)
			/* === SC_ASM_SUB_M8_<> ==== */
			X86_ASM_TEMPLATE_RMB__MEM_REG(&x86, SC_ASM_SUB_M8_R8, X86_SUB_RM8_R8, instruction)
			X86_ASM_TEMPLATE_RMB__MEM_MEM_EX(&x86, SC_ASM_SUB_M8_M8, X86_SUB_RM8_R8, instruction, X86_CFG_SUB)
			X86_ASM_TEMPLATE_RMB__MEM_ID_EX(&x86, SC_ASM_SUB_M8_I8, X86_SUB_RM8_I8, instruction, X86_CFG_SUB)


			/* ====== SC_ASM_<NOT/NEG>_<R8/M8> ======= */
			/* ====== SC_ASM_NOT_<R8/M8> ======= */
			X86_ASM_TEMPLATE_RMB__REG(&x86, SC_ASM_NOT_R8, X86_NOT_RM8, instruction, X86_CFG_NOTATION_2)
			X86_ASM_TEMPLATE_RMB__MEM(&x86, SC_ASM_NOT_M8, X86_NOT_RM8, instruction, X86_CFG_NOTATION_2)
			/* ====== SC_ASM_NEG_<R8/M8> ======= */
			X86_ASM_TEMPLATE_RMB__REG(&x86, SC_ASM_NEG_R8, X86_NOT_RM8, instruction, X86_CFG_NOTATION_3)
			X86_ASM_TEMPLATE_RMB__MEM(&x86, SC_ASM_NEG_M8, X86_NOT_RM8, instruction, X86_CFG_NOTATION_3)


			case SC_ASM_RET:
				sc_x86OpcodePush(&x86, X86_POP_R64 + RBP);
				sc_x86OpcodePush(&x86, X86_RET);
				break;

			default: SI_LOG_FMT("INSTR: %i\n", instruction->type); SI_PANIC();
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
	x86.root.registers = 0;

	ts = si_timeStampStart();

	for_range (i, 0, _startFuncLen) {
		switch (_startFunc[i]) {
			case SC_ASM_LD_R64_M64: {
				x86Register reg = sc_x86PickFunctionArg(&x86, SC_ASM_REG_PARAM_0);
				sc_x86OpcodeEx(
					&x86, X86_MOV_R32_RM32, reg, 0, RSP,
					X86_CFG_RMB | X86_CFG_SIB
				);

				break;
			}
			case SC_ASM_ARITH_R64_M64: {
				x86Register reg = sc_x86PickFunctionArg(&x86, SC_ASM_REG_PARAM_1);
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
				   	&x86, X86_MOV_RM8_R8 + 1, RDI, RAX,
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
