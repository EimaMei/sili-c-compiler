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

scOperator sc_actionHandleTokenFront(scAction* action, scTokenStruct* prev,
		scOperator operator, usize *outI, scTokenStruct** outRight) {
	usize i = *outI;

	i += 1;
	scTokenStruct* token2 = si_arrayAt(action->values, i);
	SI_ASSERT_NOT_NULL(token2);

	while (token2 && token2->type == SILEX_TOKEN_OPERATOR) {
		if (token2->token.operator == SILEX_OPERATOR_TILDE) {
			token2->token.operator = SILEX_OPERATOR_MINUS;

			if (prev && prev->type == SILEX_TOKEN_CONSTANT) {
				prev->token.constant.value.integer -= 1;
			}
		}

		if (token2->token.operator == operator) {
			operator = SILEX_OPERATOR_PLUS;
		}
		else {
			operator = SILEX_OPERATOR_MINUS;
		}

		i += 1;
		token2 = si_arrayAt(action->values, i);
	}

	*outI = i;
	*outRight = token2;
	return operator;
}
#if 0
	init->type = SC_INIT_BINARY;
	init->value.binary.left = nil;
	init->value.binary.operator = operator;
	init->value.binary.right = token2;

	prevInit = init;
	init->next = nil;
}
#endif

void sc_astNodeMakeEx(scIdentifierKey* key, siArray(scAction) action, usize i) {
	SI_ASSERT_NOT_NULL(action);

	scInitializer* prevInit = nil;
	scTokenStruct* token;

	scTokenStruct* left = nil;

	for ( ; i < si_arrayLen(action->values); i += 1) {
		token = &action->values[i];

		scInitializer* init = si_mallocItem(alloc[SC_MAIN], scInitializer);
		if (prevInit != nil) {
			prevInit->next = init;
		}
		else {
			action->init = init;
		}

		if (token->type == SILEX_TOKEN_OPERATOR) {
			if (left == nil) {
				usize ogI = i - 1;

				while (true) {
					scTokenStruct* token = si_arrayAt(action->values, i);
					i += 1;

					if (token == nil) {
						break;
					}
					else if (token->type != SILEX_TOKEN_OPERATOR) {
						left = token;
						break;
					}
				}
				SI_ASSERT_NOT_NULL(left);


				scTokenStruct* operator = si_arrayAt(action->values, i);

				if (operator == nil) {
					init->type = SC_INIT_UNARY;
					init->value.unary.operators = token;
					init->value.unary.len = left - token;
					init->value.unary.value = left;
				}
				else {
					scTokenStruct* right;
					sc_actionHandleTokenFront(action, nil, 0, &i, &right);


					scOperator op = sc_actionHandleTokenFront(action, right, operator->token.operator, &ogI, &left);

					init->type = SC_INIT_BINARY;
					init->value.binary.left = left;
					init->value.binary.operator = op;
					init->value.binary.right = right;

					prevInit = init;
					init->next = nil;

					continue;
				}
			}
			else {
				scTokenStruct* token2;
				scOperator operator = sc_actionHandleTokenFront(action, nil, token->token.operator, &i, &token2);

				init->type = SC_INIT_BINARY;
				init->value.binary.left = nil;
				init->value.binary.operator = operator;
				init->value.binary.right = token2;

				prevInit = init;
				init->next = nil;

				continue;
			}
		}
		else {
			left = token;
		}

		i += 1;

		scTokenStruct* token2 = si_arrayAt(action->values, i);
		if (token2 == nil) {
			switch (token->type) {
				case SILEX_TOKEN_CONSTANT: {
					init->type = SC_INIT_CONSTANT;
					init->value.constant = left->token.constant;
					break;
				}
				case SILEX_TOKEN_IDENTIFIER: {
					init->type = SC_INIT_IDENTIFIER;
					init->value.constant = left->token.constant;

					break;
				}
				case SILEX_TOKEN_OPERATOR: {
#if 0
					prevInit = init;

					scInitializer* init = si_mallocItem(alloc[SC_AST], scInitializer);
					prevInit->next = init;
#endif
					break;
				}
			}
			continue;
		}

		scTokenStruct* token3;
		scOperator operator = sc_actionHandleTokenFront(action, left, token2->token.operator, &i, &token3);

		init->type = SC_INIT_BINARY;
		init->value.binary.left = left;
		init->value.binary.operator = operator;
		init->value.binary.right = token3;

		prevInit = init;
		init->next = nil;

		//token2 = si_arrayBack(action->values);
		//token3 = si_arrayBack(action->values);

#if 0
		scInitializer* init = si_mallocItem(alloc[SC_AST], scInitializer);
		if (prevInit != nil) {
			prevInit->next = init;
		}
		else {
			node->init = init;
		}

		prevInit = init;
		init->next = nil;
#endif
#if 0
		switch (token->type) {
			case SILEX_TOKEN_CONSTANT: {
				//SI_STOPIF(token2 != nil, sc_actionHandleBinary(action, init, token1, token2, &i); break);
				//init->type = SC_INIT_CONSTANT;
				//init->value.constant = token->token.constant;
				break;
			}

			case SILEX_TOKEN_OPERATOR: {
				//SI_ASSERT(token2 != nil);
#if 0
				if (init == node->init) { /* NOTE(EimaMei): Jei 'init' yra apibrėžimo pradžia. */
					scTokenStruct* token3 = si_arrayAt(action->values, i + 2);
					if (token3 == nil) {
						init->type = SC_INIT_UNARY;
						init->value.unary.value = token2;
						init->value.unary.operator = token1->token.operator;

						i += 1;
						break;
					}
					sc_actionHandleTokenFront(action, token1, token3, &i);

					init->type = SC_INIT_BINARY;
					init->value.binary.left = token3;
					init->value.binary.operator = token1->token.operator;
					init->value.binary.right = token2;

					i += 3;
					break;
				}
#endif
				//init->type = SC_INIT_BINARY;
				//init->value.binary.left = nil;
				//init->value.binary.operator = token1->token.operator;
				//init->value.binary.right = token2;

				//i += 1;
				break;
			}
			case SILEX_TOKEN_IDENTIFIER: {
				//SI_STOPIF(token2 != nil, sc_actionHandleBinary(action, init, token1, token2, &i); break);
				//init->type = SC_INIT_IDENTIFIER;
				//init->value.identifier = token1->token.identifier.hash;
				break;
			}


			default: SI_PANIC();
		}
#endif

		SI_LOG("[Complete]\n");
	}
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

	if (var->init && var->init->type == SC_INIT_CONSTANT) {
		//SI_PANIC();
		//token->type = SILEX_TOKEN_CONSTANT;
		//token->token.constant = var->init->value.constant;
	}

	*res = 0;
	return var;
}
