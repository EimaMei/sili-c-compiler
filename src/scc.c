#include <scc.h>
#if defined(si_clangd_shutup)
#include <sili.h>
#include <sililex.h>
#endif


void sc_tokenGet(scLexer* lex) {
	silex_lexerTokenGet(lex);

	SI_STOPIF(lex->state == SILEX_STATE_NORMAL, return);
	if (lex->type == SILEX_TOKEN_EOF) {
		SI_PANIC();
	}

	usize len = 0;
	char buf[SI_KILO(4)];

	if (global_scope.funcName.text != nil) {
		len += si_snprintf(
			buf, sizeof(buf), "%s: In function '%*s':\n",
			global_scope.fileName, global_scope.funcName.len, global_scope.funcName.text
		) - 1;
	}
	b32 isError = (lex->error.type < SILEX_ERROR_END);

	len += si_snprintf(
		&buf[len], sizeof(buf) - len, "%s:%i:%i: %s: ",
		global_scope.fileName, lex->line, lex->column, isError ? "error" : "warning"
	) - 1;

	switch (lex->error.type) {
		case SILEX_ERROR_SUFFIX:
			len += si_snprintf(
				&buf[len], sizeof(buf) - len, "Invalid suffix \"%*s\" on an integer constant",
				lex->error.len, lex->error.data
			) - 1;
			break;
		case SILEX_WARNING_UNKNOWN_ESC_SEQUENCE:
			len += si_snprintf(
				&buf[len], sizeof(buf) - len,
				"Unknown escape sequence (Undefined behaviour)"
			) - 1;
			break;

		case SILEX_WARNING_MULTICHAR:
			len += si_snprintf(
				&buf[len], sizeof(buf) - len,
				"Multi-character character constant (Implementation-defined)"
			) - 1;
			break;

	}

	buf[len + 0] = '.';
	buf[len + 1] = '\n';

	si_fprint(SI_STDERR, buf);
	if (isError) {
		exit(1);
	}
}

scType* sc_typeGetFromKeyword(scKeyword keyword) {
	static scType* types[] = {
		&type_char, &type_short, &type_int, &type_long,
		&type_int, &type_unsigned,
		&type_float, &type_double
	};

	return types[keyword - SILEX_KEYWORD_CHAR];
}


scType* sc_typeGet(scLexer* lex, scInfoTable* scope, scKeyword* outKeyword) {
	scType* baseType;


	if (lex->type == SILEX_TOKEN_KEYWORD) {
		scKeyword keyword = lex->token.keyword;
		SI_STOPIF(!silex_keywordIsType(keyword), return nil);

		baseType = sc_typeGetFromKeyword(keyword);
		*outKeyword = keyword;
	}
	else if (lex->type == SILEX_TOKEN_IDENTIFIER) {
		scString identifier = lex->token.identifier;
		scIdentifierKey* key = si_hashtableGetWithHash(scope->identifiers, identifier.hash);
		SI_ASSERT_FMT(
			key != nil && key->type == SC_IDENTIFIER_KEY_TYPE && key->rank <= scope->rank,
			"Type '%*s' doesn't exist", identifier.len, lex->curData - identifier.len
		);
		baseType = (scType*)key->identifier;
		*outKeyword = 0;
	}
	else SI_PANIC_MSG("Illegal token for a type");

	return baseType;
}

scType sc_typeMake(scLexer* lex, scType* baseType, scKeyword keyword) {
	b32 signedModifier = false;
	b32 res;
	b32 hasSetPtr = false;
	b32 typedefed = (keyword == 0);

	scType type = *baseType;
retry:
	sc_tokenGet(lex);

	switch (lex->type) {
		case SILEX_TOKEN_PUNCTUATOR: {
			scPunctuator punct = lex->token.punctuator;
			SI_ASSERT(punct == '*');

			if (!hasSetPtr) {
				type.ptr = baseType;
				type.size = type_size_t.size;
				hasSetPtr = true;
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
					SI_ASSERT_MSG(si_betweenu(keyword, SILEX_KEYWORD_SIGNED, SILEX_KEYWORD_UNSIGNED) && !typedefed, "You cannot have multiple types.");
					SI_ASSERT_MSG(signedModifier == false, "You cannot have multiple signed modifiers.");
					SI_ASSERT_MSG(type.traits & SC_TYPE_INT, "Sign modifiers cannot be used for non-integers.");

					type = *sc_typeGetFromKeyword(lex->token.keyword);
					if (keyword == SILEX_KEYWORD_UNSIGNED) {
						type.traits |= SC_TYPE_UNSIGNED;
					}
					goto retry;
				}

				case SILEX_KEYWORD_SIGNED:
				case SILEX_KEYWORD_UNSIGNED: {
					SI_ASSERT_MSG(!typedefed, "Cannot used sign modifiers for custom types.");
					SI_ASSERT_MSG(!si_betweenu(keyword, SILEX_KEYWORD_SIGNED, SILEX_KEYWORD_UNSIGNED), "You cannot have multiple signed modifiers.");
					SI_ASSERT_MSG(type.traits & SC_TYPE_INT, "Sign modifiers cannot be used for non-integers.");

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

scType sc_typeGetAndMake(scLexer* lex, scInfoTable* scope) {
	scKeyword keyword;
	scType* baseType = sc_typeGet(lex, scope, &keyword);
	SI_STOPIF(baseType == nil, return (scType){.size = -2});

	return sc_typeMake(lex, baseType, keyword);
}


void sc_constantArithmetic(scConstant* constant, scOperator operator, scConstant src) {
	switch (operator) {
		case SILEX_OPERATOR_PLUS:
			constant->value.integer += src.value.integer;
			break;
		case SILEX_OPERATOR_MINUS:
			constant->value.integer -= src.value.integer;
			break;
		default: SI_PANIC();
	}
}

scPunctuator sc_actionAddValues(scLexer* lex, scInfoTable* scope, scAction* action) {
	scTokenStruct token;
retry:
	sc_tokenGet(lex);

	switch (lex->type) {
		case SILEX_TOKEN_PUNCTUATOR: {
			scPunctuator punctuator = lex->token.punctuator;

			if (punctuator == '(' || punctuator == ')') {
				goto push;
			}

			SI_ASSERT(punctuator == ';' || punctuator == ',');
			return punctuator;
		}

		case SILEX_TOKEN_KEYWORD: {
			if (lex->token.keyword == SILEX_KEYWORD_SIZEOF) {
				sc_tokenGet(lex);

				if (lex->type == SILEX_TOKEN_PUNCTUATOR) {
					SI_PANIC();
					SI_ASSERT(lex->token.punctuator == '(');
					sc_tokenGet(lex);
				}
				else if (lex->type == SILEX_TOKEN_IDENTIFIER) {
					i32 res;
					scVariable* key = sc_variableGet(scope, lex->token.identifier.hash, &res);
					SI_ASSERT(res == 0);

					lex->type = SILEX_TOKEN_CONSTANT;
					lex->token.constant.type = SILEX_CONSTANT_NUM_UNSIGNED;
					lex->token.constant.value.integer = key->type.size;
				}
				else SI_PANIC();
			}
			siFallthrough;
		}

		default:
push:
			token = (scTokenStruct){lex->type, lex->token};
			si_arrayPush(&action->values, token);
			goto retry;
	}

}

void sc_astHandleUnary(scAction* action, scTokenStruct* token, scAstNode* node,
		usize* outI) {
	SI_ASSERT(token->type == SILEX_TOKEN_OPERATOR);

	node->type = SC_AST_NODE_TYPE_UNARY_OP;
	node->data.unary.operator = token->token.operator;

	*outI += 1;
	token = si_arrayAt(action->values, *outI);
	SI_ASSERT_NOT_NULL(token);

	while (token->type == SILEX_TOKEN_OPERATOR) {
		SI_STOPIF(token->token.operator == SILEX_OPERATOR_PLUS, continue);

		scAstNode* init = si_mallocItem(alloc[SC_MAIN], scAstNode);
		init->type = SC_AST_NODE_TYPE_UNARY_OP;
		init->data.unary.operator = token->token.operator;

		node->data.unary.operand = init;
		node = init;

		*outI += 1;
		scTokenStruct* pToken = si_arrayAt(action->values, *outI);
		SI_STOPIF(pToken == nil, break);
		token = pToken;
	}
	SI_ASSERT(token->type == SILEX_TOKEN_IDENTIFIER);

	scAstNode* init = si_mallocItem(alloc[SC_MAIN], scAstNode);
	init->type = SC_AST_NODE_TYPE_IDENTIFIER;
	init->data.identifier = token->token.identifier;

	node->data.unary.operand = init;
}


scAstNode* sc_astTokenToNode(scAction* action, scTokenStruct* token, b32 binaryOP,
		scAstNode* prevNode, usize* outI) {
	scAstNode* node	= si_mallocItem(alloc[SC_MAIN], scAstNode);

	switch (token->type) {
		case SILEX_TOKEN_CONSTANT: {
			node->type = SC_AST_NODE_TYPE_CONSTANT;
			node->data.constant = token->token.constant;
			break;
		}
		case SILEX_TOKEN_IDENTIFIER: {
			node->type = SC_AST_NODE_TYPE_IDENTIFIER;
			node->data.identifier = token->token.identifier;
			break;
		}
		case SILEX_TOKEN_OPERATOR: {
			if (prevNode == nil || binaryOP) {
				sc_astHandleUnary(action, token, node, outI);
				break;
			}

			*outI += 1;
			scTokenStruct* nextToken = si_arrayAt(action->values, *outI);
			SI_ASSERT_NOT_NULL(nextToken);
			scAstNode* nextNode = sc_astTokenToNode(action, nextToken, true, prevNode, outI);

			node->type = SC_AST_NODE_TYPE_BINARY_OP;
			node->data.binary.left = prevNode;
			node->data.binary.operator = token->token.operator;
			node->data.binary.right = nextNode;

			break;
		}
		case SILEX_TOKEN_PUNCTUATOR: {
			if (token->token.punctuator != '(') {
				SI_PANIC();
			}

			node->type = SC_AST_NODE_TYPE_GROUP_OP;
			prevNode = node;

			while (true) {
				*outI += 1;
				token = si_arrayAt(action->values, *outI);
				SI_ASSERT_NOT_NULL(token);
				if (token->type == SILEX_TOKEN_PUNCTUATOR && token->token.punctuator == ')') {
					break;
				}
				prevNode = sc_astTokenToNode(action, token, false, prevNode, outI);
			}
			node->data.group.start = prevNode;


			break;
		}
		default: SI_LOG_FMT("%i %i\n", token->type, token->token.keyword); SI_PANIC();
	}

	return node;
}
#if 0
*outI += 1;
token = si_arrayAt(action->values, *outI);
SI_ASSERT_NOT_NULL(token);

node->type = SC_AST_NODE_TYPE_BINARY_OP;
node->data.binary.left = sc_astTokenToNode(action, token, binaryOP, prevNode, outI);

*outI += 1;
token = si_arrayAt(action->values, *outI);
SI_ASSERT_NOT_NULL(token);
SI_ASSERT(token->type == SILEX_TOKEN_OPERATOR);
node->data.binary.operator = token->token.operator;

*outI += 1;
token = si_arrayAt(action->values, *outI);
SI_ASSERT_NOT_NULL(token);
node->data.binary.right = sc_astTokenToNode(action, token, binaryOP, node->data.binary.left, outI);

*outI += 1;
token = si_arrayAt(action->values, *outI);
prevNode = node;
// goto skip;
while (token && !(token->type == SILEX_TOKEN_PUNCTUATOR && token->token.punctuator == ')')) {
	prevNode = sc_astTokenToNode(action, token, false, prevNode, outI);

	*outI += 1;
	token = si_arrayAt(action->values, *outI);
#if 0
	node = si_mallocItem(alloc[SC_MAIN], scAstNode);
skip:
	node->type = SC_AST_NODE_TYPE_BINARY_OP;
	node->data.binary.left = sc_astTokenToNode(action, token, binaryOP, prevNode, outI);

	*outI += 1;
	token = si_arrayAt(action->values, *outI);
	SI_ASSERT_NOT_NULL(token);
	SI_ASSERT(token->type == SILEX_TOKEN_OPERATOR);
	node->data.binary.operator = token->token.operator;

	*outI += 1;
	token = si_arrayAt(action->values, *outI);
	SI_ASSERT_NOT_NULL(token);
	node->data.binary.right = sc_astTokenToNode(action, token, binaryOP, node->data.binary.left, outI);
	prevNode = node;


	*outI += 1;
	token = si_arrayAt(action->values, *outI);
	SI_ASSERT_NOT_NULL(token);
#endif

}
SI_ASSERT_NOT_NULL(token);
#endif

void sc_astNodeMake(siArray(scAction) action, b32 firstIsIdentifier) {
	SI_ASSERT_NOT_NULL(action);

	scAstNode* prevNode = nil;
	for_range (i, firstIsIdentifier, si_arrayLen(action->values)) {
		scTokenStruct* token = &action->values[i];
		prevNode = sc_astTokenToNode(action, token, false, prevNode, &i);
	}

	action->root = prevNode;
}

scVariable* sc_variableGet(scInfoTable* scope, u64 hash, i32* res) {
	scIdentifierKey* key = si_hashtableGetWithHash(
		scope->identifiers, hash
	);
	SI_STOPIF(key == nil, *res = 2; goto end);

	scVariable* var = (scVariable*)key->identifier;
	SI_STOPIF(key->type != SC_IDENTIFIER_KEY_VAR, *res = 3; goto end);
	SI_STOPIF(key->rank == UINT16_MAX, *res = 4; goto end);

	*res = 0;
	return var;

end:
	return nil;
}

scVariable* sc_variableGetAndOptimizeToken(scInfoTable* scope, scTokenStruct* token, i32* res) {
	SI_STOPIF(token->type != SILEX_TOKEN_IDENTIFIER, *res = 1; return nil);

	scVariable* var = sc_variableGet(scope, token->token.identifier.hash, res);
	SI_STOPIF(var == nil, return nil);
#if 0
	if (var->init && var->init->type == SC_INIT_CONSTANT) {
		//SI_PANIC();
		//token->type = SILEX_TOKEN_CONSTANT;
		//token->token.constant = var->init->value.constant;
	}
#endif

	*res = 0;
	return var;
}

scAsmRegister sc_asmRegisterAny(scAsmEnvironmentState* state, b32 set) {
	scAsmRegister reg = SC_ASM_REG_0;
	while (!sc_asmRegisterAvailable(*state, reg)) {
		reg += 1;
	}

	if (set) {
		sc_asmRegisterSet(state, reg);
	}

	return reg;
}

void sc_asmRegisterSet(scAsmEnvironmentState* state, scAsmRegister reg) {
	SI_ASSERT(si_betweenu(reg, SC_ASM_REG_0, SC_ASM_REG_15));

	u32 value = reg - SC_ASM_REG_0;
	state->registers |= SI_BIT(value);
}

void sc_asmRegisterUnset(scAsmEnvironmentState* state, scAsmRegister reg) {
	SI_ASSERT(si_betweenu(reg, SC_ASM_REG_0, SC_ASM_REG_15));

	u32 value = reg - SC_ASM_REG_0;
	state->registers &= ~SI_BIT(value);
}



b32 sc_asmRegisterAvailable(scAsmEnvironmentState state, scAsmRegister reg) {
	SI_ASSERT(si_betweenu(reg, SC_ASM_REG_0, SC_ASM_REG_15));

	u32 value = reg - SC_ASM_REG_0;
	u32 regBit = SI_BIT(value);

	if ((state.registers & regBit) == 0) {
		return true;
	}

	return false;
}
