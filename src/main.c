#define SILEX_USE_HASH
#define SILEX_NO_LEN
#include <sili.h>
#include <sililex.h>

#include <scc.h>
#include <x86.h>
#include <exegen.h>

/* Source */
#include <scc/scc.c>


usize sizeof_SIZE_T = 8;

scType type_char = (scType){1, SC_TYPE_INT, 0, nil};
scType type_short = (scType){2, SC_TYPE_INT, 0, nil};
scType type_int = (scType){4, SC_TYPE_INT, 0, nil};
scType type_long = (scType){8, SC_TYPE_INT, 0, nil};
scType type_unsigned = (scType){4, SC_TYPE_INT | SC_TYPE_UNSIGNED, 0, nil};
scType type_float = (scType){4, SC_TYPE_FLOAT, 0, nil};
scType type_double = (scType){8, SC_TYPE_FLOAT, 0, nil};


#define SC_MAX_FUNCS 128

#define SC_MAX_VARS 128
#define SC_MAX_ACTIONS 128
#define SC_MAX_INITIALIZERS 32

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
		SI_MEGA(2),
		(3 * sizeof(siArrayHeader) + sizeof(siHashEntry) * SC_MAX_VARS) * SC_MAX_FUNCS +
		(sizeof(scFunction)) * SC_MAX_FUNCS +
		((sizeof(scVariable)) * SC_MAX_VARS) * SC_MAX_FUNCS +
		(sizeof(scAction) * SC_MAX_ACTIONS) * SC_MAX_FUNCS +
		(sizeof(scTokenStruct) * SC_MAX_INITIALIZERS) * SC_MAX_ACTIONS
	);
	SC_ALLOCATOR_MAKE(
		SC_SCOPE,
		SI_KILO(1),
		SI_KILO(1)
	);

	scInfoTable global_scope;
	/* NOTE(EimaMei): Pagal struktūros 'scInfoTable' komentarą 'include/scc.h' faile. */
	global_scope.vars = si_malloc(
		alloc[SC_MAIN],
		sizeof(scIdentifierKeyType) * (SC_MAX_VARS + SC_MAX_FUNCS) +
		sizeof(scVariable) * SC_MAX_VARS + sizeof(scFunction) * SC_MAX_FUNCS
	);
	global_scope.types = nil;
	global_scope.stack = 0;
	global_scope.parent = nil;
	global_scope.identifiers = si_hashtableMakeReserve(alloc[SC_MAIN], SC_MAX_VARS + SC_MAX_FUNCS);


	scInfoTable* scope = &global_scope;
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
	b32 isInFunc = false;

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

							scFunction* func = sc_globalScopeFuncPtr(scope, si_arrayLen(scope->identifiers), SC_MAX_VARS);
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

							b32 res = silex_lexerTokenGet(&lex);
							SI_ASSERT(res && lex.type == SILEX_TOKEN_PUNCTUATOR);

							switch (lex.token.punctuator) {
								case '{': isInFunc = true; curFunc = func; break;
								case ';': break;
								default: SI_PANIC_MSG("Cannot use this punctuator after a function declaration");
							}


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

			case SILEX_TOKEN_PUNCTUATOR: {
				switch (lex.token.punctuator) {
					case '}': {
						if (isInFunc) {
							isInFunc = false;
							curFunc = &scope_GLOBAL;
						}
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

		var->stack = curFunc->stack;
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
									var->stack = curFunc->stack;

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
						case SC_INIT_IDENTIFIER: {
							scVariable* var = si_hashtableGetWithHash(curFunc->scope, init->value.identifier.hash);
							switch (curFunc->type.size) {
								case 4: {
									asm.type = SC_ASM_RET_M32;
									asm.src = var->stack;

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

	alloc[SC_X86ASM] = si_allocatorMake(2 * sizeof(u8) * 16 * si_arrayLen(instructions));
	bytes = alloc[SC_X86ASM]->maxLen;
	si_printf("%f MB\n", bytes / 1024.f / 1024.f);
	SI_ASSERT(bytes < SI_MEGA(1));


	usize x86Len = 0;
	u8* x86 = si_mallocArray(alloc[SC_X86ASM], u8, 16 * si_arrayLen(instructions));

	ts = si_timeStampStart();


	x86EnvironmentState x86state;
	x86state.conv = X86_CALLING_CONV_SYSTEM_V_X86;
	x86state.registers = 0;
	usize offset = sizeof(elf64ElfHeader) + sizeof(elf64ProgramHeader);

	ts = si_timeStampStart();
	si_timeStampPrintSince(ts);

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
					x86[x86Len + len + 1] = X86_SYSCALL_H;
					x86[x86Len + len + 2] = X86_SYSCALL_L;
					len += 3;
				}

				x86Len += len;
				break;
			}

			case SC_ASM_RET_M32: {
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
						X86_MOV_R32_RM32,
						X86_CFG_RMB | X86_CFG_DST_R | X86_CFG_SRC_M,
						EDI,
						instruction->src,
						&x86[x86Len + len]
					);

					x86[x86Len + len] = X86_POP_R64 + RBP;
					x86[x86Len + len + 1] = X86_SYSCALL_H;
					x86[x86Len + len + 2] = X86_SYSCALL_L;
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
