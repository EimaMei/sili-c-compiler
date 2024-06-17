#include <scc.h>
#if defined(si_clangd_shutup)
#include <sili.h>
#include <sililex.h>
#endif

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
	res = silex_lexerTokenGet(lex);
	SI_ASSERT(res);

	switch (lex->type) {
		case SILEX_TOKEN_PUNCTUATOR: {
			scPunctuator punct = lex->token.punctuator;
			SI_ASSERT(punct == '*');

			if (!hasSetPtr) {
				type.ptr = baseType;
				type.size = sizeof_SIZE_T;
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

scPunctuator sc_actionAddValues(scLexer* lex, scAction* action) {
	scTokenStruct token;
	b32 res;
retry:
	res = silex_lexerTokenGet(lex);
	SI_ASSERT(res);

	switch (lex->type) {
		case SILEX_TOKEN_PUNCTUATOR: {
			scPunctuator punctuator = lex->token.punctuator;
			SI_STOPIF(punctuator == '(' || punctuator == ')', goto retry);
			SI_ASSERT(punctuator == ';' || punctuator == ',');

			return punctuator;
		}
		default:
			token = (scTokenStruct){lex->type, lex->token};
			si_arrayPush(&action->values, token);
			goto retry;
	}

}

void sc_astHandleUnary(scAction* action, scTokenStruct* token, scAstNode* nextNode,
		scTokenStruct* nextToken, usize* outI) {
	nextNode->type = SC_AST_NODE_TYPE_UNARY_OP;
	nextNode->data.unary.operator = token->token.operator;

	b32 valid = false;
	while (nextToken && nextToken->type == SILEX_TOKEN_OPERATOR) {
		if (nextToken->token.operator == SILEX_OPERATOR_PLUS) {
			continue;
		}
		valid = true;

		scAstNode* init = si_mallocItem(alloc[SC_MAIN], scAstNode);
		init->type = SC_AST_NODE_TYPE_UNARY_OP;
		init->data.unary.operator = nextToken->token.operator;

		nextNode->data.unary.operand = init;
		nextNode = init;

		*outI += 1;
		nextToken = si_arrayAt(action->values, *outI);
	}
	SI_ASSERT_NOT_NULL(nextToken);

	if (valid) {
		scAstNode* init = si_mallocItem(alloc[SC_MAIN], scAstNode);
		init->type = SC_AST_NODE_TYPE_IDENTIFIER;
		init->data.identifier = nextToken->token.identifier.hash;

		nextNode->data.unary.operand = init;
	}
	else {
		nextNode->type = SC_AST_NODE_TYPE_IDENTIFIER;
		nextNode->data.identifier = nextToken->token.identifier.hash;
	}
}

void sc_astNodeMake(siArray(scAction) action, b32 firstIsIdentifier) {
	SI_ASSERT_NOT_NULL(action);

	scAstNode* prevNode = nil;
	for_range (i, firstIsIdentifier, si_arrayLen(action->values)) {
		scTokenStruct* token = &action->values[i];
		scAstNode* node	= si_mallocItem(alloc[SC_MAIN], scAstNode);

		switch (token->type) {
			case SILEX_TOKEN_CONSTANT: {
				node->type = SC_AST_NODE_TYPE_CONSTANT;
				node->data.constant = token->token.constant;
				break;
			}
			case SILEX_TOKEN_IDENTIFIER: {
				node->type = SC_AST_NODE_TYPE_IDENTIFIER;
				node->data.identifier = token->token.identifier.hash;
				break;
			}
			case SILEX_TOKEN_OPERATOR: {
				i += 1;
				scTokenStruct* nextToken = si_arrayAt(action->values, i);
				SI_ASSERT_NOT_NULL(nextToken);
				scAstNode* nextNode = si_mallocItem(alloc[SC_MAIN], scAstNode);

				if (prevNode == nil) {
					sc_astHandleUnary(action, token, nextNode, nextToken, &i);
					prevNode = nextNode;

					continue;
				}

				switch (nextToken->type) {
					case SILEX_TOKEN_CONSTANT:
						nextNode->type = SC_AST_NODE_TYPE_CONSTANT;
						nextNode->data.constant = nextToken->token.constant;
						break;
					case SILEX_TOKEN_IDENTIFIER:
						nextNode->type = SC_AST_NODE_TYPE_IDENTIFIER;
						nextNode->data.identifier = nextToken->token.identifier.hash;
						break;
					case SILEX_TOKEN_OPERATOR:
						i += 1;
						sc_astHandleUnary(action, nextToken, nextNode, si_arrayAt(action->values, i), &i);
						break;

					default: SI_PANIC();
				}
				node->type = SC_AST_NODE_TYPE_BINARY_OP;
				node->data.binary.left = prevNode;
				node->data.binary.operator = token->token.operator;
				node->data.binary.right = nextNode;
				break;
			}
			default: SI_PANIC();
		}

		prevNode = node;
	}

	action->root = prevNode;

#if 0
		switch (token->type) {
			case SILEX_TOKEN_OPERATOR: {
				if (unaryState == false) {
					init->operator = token->token.operator;
					init->node.binary.left = prevInit;
					unaryState = true;

					if (prevInit->operator != 0) {
						prevInit->parent = init;
					}
					break;
				}

				scInitializer* ogPrevInit = (prevInit->operator != 0)
					? prevInit
					: init;
				scInitializer* start = init;

				init->operator = token->token.operator;
				prevInit = init;
				i += 1;

				while (true) {
					token = &action->values[i];
					SI_STOPIF(token->type != SILEX_TOKEN_OPERATOR, break);

					init = si_mallocItem(alloc[SC_MAIN], scInitializer);
					init->operator = token->token.operator;
					init->parent = start;

					prevInit->node.binary.left = init;
					prevInit->node.binary.right = nil;

					prevInit = init;
					i += 1;
				}

				scInitializer* leftInit = si_mallocItem(alloc[SC_MAIN], scInitializer);
				leftInit->operator = 0;
				leftInit->node.value = token;
				leftInit->parent = start;

				prevInit->node.binary.left = leftInit;
				prevInit->node.binary.right = nil;

				i += 1;
				token = si_arrayAt(action->values, i);

				if (token != nil) {
					init = si_mallocItem(alloc[SC_MAIN], scInitializer);
					init->operator = token->token.operator;
					init->node.binary.left = start;
					init->parent = ogPrevInit;
					ogPrevInit->node.binary.right = init;
				}
				else {
					ogPrevInit->node.binary.right = start;
				}

				unaryState = true;

				break;
			}

			default: {
				init->operator = 0;
				init->node.value = token;
				unaryState = false;

				if (prevInit->operator != 0) {
					prevInit->node.binary.right = init;
					init = prevInit;
				}
			}
		}

		prevInit = init;
#endif
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
