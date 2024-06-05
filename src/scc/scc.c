#include <scc.h>


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
