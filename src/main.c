#include <sili.h>

cstring keywords[] = {
	"auto", "break", "case", "char", "const", "continue", "default", "do",
	"double", "else", "enum", "extern", "float", "for", "goto", "if", "int", "long",
	"register", "return", "short", "signed", "sizeof", "static", "struct", "switch",
	"typedef", "union", "unsigned", "void", "volatile", "while"
};
u64 keywords_U64[countof(keywords)];

typedef SI_ENUM(u32, scTokenType) {
	SILEX_TOKEN_NONE = 0,
	SILEX_TOKEN_KEYWORD,
	SILEX_TOKEN_IDENTIFIER,
	SILEX_TOKEN_CONSTANT,
	SILEX_TOKEN_STRING_LITERAL,
	SILEX_TOKEN_OPERATOR,
	SILEX_TOKEN_PUNCTUATOR,

	SILEX_TOKEN_EOF,
	SILEX_TOKEN_INVALID,
};


typedef SI_ENUM(u32, siTokenError) {
	SILEX_ERROR_NONE,
	SILEX_ERROR_SUFFIX_LONG,
	SILEX_ERROR_SUFFIX_UNSIGNED,
	SILEX_ERROR_PREFIX_MINUS,
};

typedef SI_ENUM(u32, siKeyword) {
	SILEX_KEYWORD_NONE = 0,
	SILEX_KEYWORD_AUTO,
	SILEX_KEYWORD_BREAK,
	SILEX_KEYWORD_CASE,
	SILEX_KEYWORD_CONST,
	SILEX_KEYWORD_CONTINUE,
	SILEX_KEYWORD_DEFAULT,
	SILEX_KEYWORD_DO,
	SILEX_KEYWORD_ELSE,
	SILEX_KEYWORD_ENUM,
	SILEX_KEYWORD_EXTERN,
	SILEX_KEYWORD_FOR,
	SILEX_KEYWORD_GOTO,
	SILEX_KEYWORD_IF,
	SILEX_KEYWORD_REGISTER,
	SILEX_KEYWORD_RETURN,
	SILEX_KEYWORD_SIZEOF,
	SILEX_KEYWORD_STATIC,
	SILEX_KEYWORD_STRUCT,
	SILEX_KEYWORD_SWITCH,
	SILEX_KEYWORD_TYPEDEF,
	SILEX_KEYWORD_UNION,
	SILEX_KEYWORD_VOID,
	SILEX_KEYWORD_VOLATILE,
	SILEX_KEYWORD_WHILE,

	/* Type keywords */
	SILEX_KEYWORD_CHAR,
	SILEX_KEYWORD_SHORT,
	SILEX_KEYWORD_INT,
	SILEX_KEYWORD_LONG,
	SILEX_KEYWORD_SIGNED,
	SILEX_KEYWORD_UNSIGNED,
	SILEX_KEYWORD_FLOAT,
	SILEX_KEYWORD_DOUBLE,
};

#define silex_keywordIsType(keyword) si_betweenu(keyword, SILEX_KEYWORD_CHAR, SILEX_KEYWORD_DOUBLE)

typedef SI_ENUM(u32, siConstantType) {
	SILEX_CONSTANT_NUM_SIGNED = 1,
	SILEX_CONSTANT_NUM_UNSIGNED,
	SILEX_CONSTANT_FLOAT,
};

typedef struct {
	siConstantType type;
	union {
		i64 _signed;
		u64 _unsigned;
		f64 _float;
	} value;
} siConstant;

typedef SI_ENUM(u32, siPunctuator) {
	SILEX_PUNCTUATOR_BRACKET_L = '(',
	SILEX_PUNCTUATOR_BRACKET_R = ')',
	SILEX_PUNCTUATOR_SQUARE_BRACKET_L = '[',
	SILEX_PUNCTUATOR_SQUARE_BRACKET_R = ']',
	SILEX_PUNCTUATOR_CURLY_BRACKET_L = '{',
	SILEX_PUNCTUATOR_CURLY_BRACKET_R = '}',

	SILEX_PUNCTUATOR_SEMICOLON = ';',
	SILEX_PUNCTUATOR_EQUAL = '=',

	SILEX_PUNCTUATOR_COMMA = ',',
};

typedef SI_ENUM(u32, siOperator) {
	SILEX_OPERATOR_PLUS,
	SILEX_OPERATOR_PLUSPLUS,
	SILEX_OPERATOR_MINUS,
	SILEX_OPERATOR_MINUSMINUS,
};


#define SILEX_USE_HASH 1
#define SILEX_NO_LEN 1

#if defined(SILEX_USE_HASH)
	#ifndef SILEX_HASH_TYPE
		#define SILEX_HASH_TYPE u64
	#endif

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
		#define SILEX_HASH_FUNC_END(hash, str, index)
	#endif


	typedef SILEX_HASH_TYPE silexHashType;
#else
	#define SILEX_HASH_FUNC_INIT(hash)
	#define SILEX_HASH_FUNC(hash, character)
	#define SILEX_HASH_FUNC_END(hash, str, index)
#endif


typedef struct {
#if !defined(SILEX_USE_HASH)
	cstring text;
#else
	silexHashType hash;
#endif
#if !defined(SILEX_NO_LEN)
	usize len;
#endif
} scString;

typedef union {
	scString text;
	siKeyword keyword;
	siPunctuator punctuator;
	siConstant constant;
	siOperator operator;
} scToken;

typedef struct {
	cstring curData;
	cstring end;

	scTokenType token;
	scToken value;

	siTokenError error;
} scLexer;

#if 1
#define SILEX__TOKENID_AUTO 0x6F747561
#define SILEX__TOKENID_BREAK 0x6B61657262
#define SILEX__TOKENID_CASE 0x65736163
#define SILEX__TOKENID_CHAR 0x72616863
#define SILEX__TOKENID_CONST 0x74736E6F63
#define SILEX__TOKENID_CONTINUE 0x65756E69746E6F63
#define SILEX__TOKENID_DEFAULT 0x746C7561666564
#define SILEX__TOKENID_DO 0x6F64
#define SILEX__TOKENID_DOUBLE 0x656C62756F64
#define SILEX__TOKENID_ELSE 0x65736C65
#define SILEX__TOKENID_ENUM 0x6D756E65
#define SILEX__TOKENID_EXTERN 0x6E7265747865
#define SILEX__TOKENID_FLOAT 0x74616F6C66
#define SILEX__TOKENID_FOR 0x726F66
#define SILEX__TOKENID_GOTO 0x6F746F67
#define SILEX__TOKENID_IF 0x6669
#define SILEX__TOKENID_INT 0x746E69
#define SILEX__TOKENID_LONG 0x676E6F6C
#define SILEX__TOKENID_REGISTER 0x7265747369676572
#define SILEX__TOKENID_RETURN 0x6E7275746572
#define SILEX__TOKENID_SHORT 0x74726F6873
#define SILEX__TOKENID_SIGNED 0x64656E676973
#define SILEX__TOKENID_SIZEOF 0x666F657A6973
#define SILEX__TOKENID_STATIC 0x636974617473
#define SILEX__TOKENID_STRUCT 0x746375727473
#define SILEX__TOKENID_SWITCH 0x686374697773
#define SILEX__TOKENID_TYPEDEF 0x66656465707974
#define SILEX__TOKENID_UNION 0x6E6F696E75
#define SILEX__TOKENID_UNSIGNED 0x64656E6769736E75
#define SILEX__TOKENID_VOID 0x64696F76
#define SILEX__TOKENID_VOLATILE 0x656C6974616C6F76
#define SILEX__TOKENID_WHILE 0x656C696877
#endif
siKeyword silex_keywordGet(u64 num) {
	switch (num) {
		case SILEX__TOKENID_AUTO: return SILEX_KEYWORD_AUTO;
		case SILEX__TOKENID_BREAK: return SILEX_KEYWORD_BREAK;
		case SILEX__TOKENID_CASE: return SILEX_KEYWORD_CASE;
		case SILEX__TOKENID_CHAR: return SILEX_KEYWORD_CHAR;
		case SILEX__TOKENID_CONST: return SILEX_KEYWORD_CONST;
		case SILEX__TOKENID_CONTINUE: return SILEX_KEYWORD_CONTINUE;
		case SILEX__TOKENID_DEFAULT: return SILEX_KEYWORD_DEFAULT;
		case SILEX__TOKENID_DO: return SILEX_KEYWORD_DO;
		case SILEX__TOKENID_DOUBLE: return SILEX_KEYWORD_DOUBLE;
		case SILEX__TOKENID_ELSE: return SILEX_KEYWORD_ELSE;
		case SILEX__TOKENID_ENUM: return SILEX_KEYWORD_ENUM;
		case SILEX__TOKENID_EXTERN: return SILEX_KEYWORD_EXTERN;
		case SILEX__TOKENID_FLOAT: return SILEX_KEYWORD_FLOAT;
		case SILEX__TOKENID_FOR: return SILEX_KEYWORD_FOR;
		case SILEX__TOKENID_GOTO: return SILEX_KEYWORD_GOTO;
		case SILEX__TOKENID_IF: return SILEX_KEYWORD_IF;
		case SILEX__TOKENID_INT: return SILEX_KEYWORD_INT;
		case SILEX__TOKENID_LONG: return SILEX_KEYWORD_LONG;
		case SILEX__TOKENID_REGISTER: return SILEX_KEYWORD_REGISTER;
		case SILEX__TOKENID_RETURN: return SILEX_KEYWORD_RETURN;
		case SILEX__TOKENID_SHORT: return SILEX_KEYWORD_SHORT;
		case SILEX__TOKENID_SIGNED: return SILEX_KEYWORD_SIGNED;
		case SILEX__TOKENID_SIZEOF: return SILEX_KEYWORD_SIZEOF;
		case SILEX__TOKENID_STATIC: return SILEX_KEYWORD_STATIC;
		case SILEX__TOKENID_STRUCT: return SILEX_KEYWORD_STRUCT;
		case SILEX__TOKENID_SWITCH: return SILEX_KEYWORD_SWITCH;
		case SILEX__TOKENID_TYPEDEF: return SILEX_KEYWORD_TYPEDEF;
		case SILEX__TOKENID_UNION: return SILEX_KEYWORD_UNION;
		case SILEX__TOKENID_UNSIGNED: return SILEX_KEYWORD_UNSIGNED;
		case SILEX__TOKENID_VOID: return SILEX_KEYWORD_VOID;
		case SILEX__TOKENID_VOLATILE: return SILEX_KEYWORD_VOLATILE;
		case SILEX__TOKENID_WHILE: return SILEX_KEYWORD_WHILE;
	}

	return SILEX_KEYWORD_NONE;
}


scLexer silex_lexerMake(cstring content, usize len) {
	scLexer lexer = {0};
	lexer.curData = content;
	lexer.end = content + len;

	return lexer;
}


b32 silex_lexerTokenGet(scLexer* lexer) {
	SI_STOPIF(lexer->curData >= lexer->end, lexer->token = SILEX_TOKEN_EOF; return false);

	const char* pLetter = lexer->curData;
	while (si_charIsSpace(*pLetter)) { pLetter += 1; }
	SI_STOPIF(pLetter >= lexer->end, lexer->token = SILEX_TOKEN_EOF; return false);


	switch (*pLetter) {
		default: {
			if (si_charIsAlpha(*pLetter) || *pLetter == '_' || *(u8*)pLetter >= 128) {
				SILEX_HASH_FUNC_INIT(hash);
				const char* start = pLetter;

				while (si_charIsAlphanumeric(*pLetter)) {
					SILEX_HASH_FUNC(hash, *pLetter);
					pLetter += 1;
				}
				lexer->curData = pLetter;

				usize len = pLetter - start;

				if (len <= 8) {
					u64 stringInt = 0;
					memcpy(&stringInt, start, len);

					siKeyword keyword = silex_keywordGet(stringInt);
					if (keyword != SILEX_KEYWORD_NONE) {
						lexer->token = SILEX_TOKEN_KEYWORD;
						lexer->value.keyword = keyword;
						return true;
					}
				}

				lexer->token = SILEX_TOKEN_IDENTIFIER;
#ifndef SILEX_NO_LEN
				lexer->value.text.len = len;
#endif
#ifndef SILEX_USE_HASH
				lexer->value.text.text = start;
#else
				lexer->value.text.hash = hash;
#endif

				return true;
			}
			siFallthrough;
		}

		case '+': {
			while (si_charIsSpace(*pLetter)) { pLetter += 1; }

			 if (*pLetter == '+') {
				lexer->curData = pLetter + 1;
				lexer->token = SILEX_TOKEN_OPERATOR;
				lexer->value.operator = SILEX_OPERATOR_PLUSPLUS;
				return true;
			}

			lexer->curData = pLetter;
			lexer->token = SILEX_TOKEN_OPERATOR;
			lexer->value.operator = SILEX_OPERATOR_PLUS;
			return true;
		}


		case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7':
		case '8': case '9': {
			b32 state = 0; /* NOTE(EimaMei): SI_BIT(0) - is unsigned, SI_BIT(1) - is long, SI_BIT(2) - is negative. */
			u64 value = 0;
			u32 base = 10;

			while (true) {
				char x = si_charLower(*pLetter);

				switch (x) {
					case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9':
						value *= base;
						value += (x - '0');
						pLetter += 1;
						continue;
					case 'l': case 'L': {
						if ((state & SI_BIT(1)) == 0) {
							state |= SI_BIT(1);
							pLetter += 1;
							continue;
						}

						lexer->curData = pLetter;
						lexer->token = SILEX_TOKEN_INVALID;
						lexer->error = SILEX_ERROR_SUFFIX_LONG;
						return false;
					}
					case 'u': case 'U': {
						if ((state & SI_BIT(0)) == 0) {
							state |= SI_BIT(0);
							pLetter += 1;
							continue;
						}

						lexer->curData = pLetter;
						lexer->token = SILEX_TOKEN_INVALID;
						lexer->error = SILEX_ERROR_SUFFIX_UNSIGNED;
						return false;
					}
					case '-': {
						if ((state & SI_BIT(2)) == 0) {
							state |= SI_BIT(2);
							pLetter += 1;
							continue;
						}

						lexer->curData = pLetter;
						lexer->token = SILEX_TOKEN_INVALID;
						lexer->error = SILEX_ERROR_PREFIX_MINUS;
						return false;
					}
				}

				break;
			}
			lexer->curData = pLetter;
			lexer->token = SILEX_TOKEN_CONSTANT;

			siConstant* constant = &lexer->value.constant;
			if (state & SI_BIT(2)) {
				value = -value;
			}
			if (state & SI_BIT(0)) {
				constant->value._unsigned = value;
				constant->type = SILEX_CONSTANT_NUM_UNSIGNED;
			}
			else {
				constant->value._signed = value;
				constant->type = SILEX_CONSTANT_NUM_SIGNED;
			}

			return true;
		}

		case '*':
			lexer->curData = pLetter + 1;

			if (lexer->token == SILEX_TOKEN_KEYWORD) {
				b32 valid = silex_keywordIsType(lexer->value.keyword);
				SI_ASSERT(valid);

				lexer->token = SILEX_TOKEN_PUNCTUATOR;
				lexer->value.punctuator = '*';
			}

			return true;

		case '(': case ')': case '[': case ']': case '{': case '}': case ';': case '=':
		case ',':
			lexer->curData = pLetter + 1;

			lexer->token = SILEX_TOKEN_PUNCTUATOR;
			lexer->value.punctuator = *pLetter;
			return true;
	}
	SI_PANIC();

	return false;
}



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
	SC_ACTION_VAR_MAKE = 1,
	SC_ACTION_VAR_ASSIGN_CONSTANT,
	SC_ACTION_VAR_ADD_CONSTANT,
	SC_ACTION_RETURN,
};

typedef struct scAction {
	scActionType type;
	u32 nextActionIndex;
	union {
		scVariable* var;
		scToken token;
	} value;
} scAction;
SI_STATIC_ASSERT(sizeof(scAction) == 24);


typedef struct {
	scType type;
    scString name;
    siArray(scVariable) parameters;
    siArray(scVariable) scope;
	siArray(scAction) code;
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

scType* sc_typeGetFromKeyword(siKeyword keyword) {
	static scType* types[] = {
		&type_char, &type_short, &type_int, &type_long,
		&type_int, &type_unsigned,
		&type_float, &type_double
	};

	return types[keyword - SILEX_KEYWORD_CHAR];
}


scType sc_typeGet(scLexer* lex) {
	SI_ASSERT(lex->token == SILEX_TOKEN_KEYWORD);

	siKeyword keyword = lex->value.keyword;
	SI_STOPIF(!silex_keywordIsType(keyword), return (scType){.size = -1});

	scType* baseType = sc_typeGetFromKeyword(keyword);
	scType type = *baseType;

	b32 signedModifier = false;
	b32 res;
retry:
	res = silex_lexerTokenGet(lex);
	SI_ASSERT(res);

	switch (lex->token) {
		case SILEX_TOKEN_PUNCTUATOR: {
			siPunctuator punct = lex->value.punctuator;
			SI_ASSERT(punct == '*');

			if (type.ptrCount == 0) {
				type.ptr = baseType;
				type.size = sizeof_SIZE_T;
			}
			type.ptrCount += 1;
			goto retry;
		}

		case SILEX_TOKEN_KEYWORD: {
			switch (lex->value.keyword) {
				case SILEX_KEYWORD_CHAR:
				case SILEX_KEYWORD_SHORT:
				case SILEX_KEYWORD_INT:
				case SILEX_KEYWORD_LONG: {
					SI_ASSERT_MSG(si_betweenu(keyword, SILEX_KEYWORD_SIGNED, SILEX_KEYWORD_UNSIGNED), "You cannot have multiple types.");
					SI_ASSERT_MSG(signedModifier == false, "You cannot have multiple signed modifiers.");
					SI_ASSERT_MSG(type.traits & SC_TYPE_INT, "Signed modifiers cannot be used for non-integers.");

					type = *sc_typeGetFromKeyword(lex->value.keyword);
					if (keyword == SILEX_KEYWORD_UNSIGNED) {
						type.traits |= SC_TYPE_UNSIGNED;
					}
					goto retry;
				}

				case SILEX_KEYWORD_UNSIGNED: {
					SI_ASSERT_MSG(!si_betweenu(keyword, SILEX_KEYWORD_SIGNED, SILEX_KEYWORD_UNSIGNED), "You cannot have multiple signed modifiers.");
					SI_ASSERT_MSG(type.traits & SC_TYPE_INT, "Signed modifiers cannot be used for non-integers.");

					if (lex->value.keyword == SILEX_KEYWORD_UNSIGNED) {
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
	SC_MAIN,

	SC_FILE,
	SC_FUNC,
	SC_VARS,

	SC_ALLOC_LEN
};


#define SC_MAX_FUNCS 128
#define SC_MAX_ACTIONS 128
#define SC_MAX_PARAM 128
#define SC_MAX_VARS 256



int main(void) {
	for_range (i, 0, countof(keywords)) {
		usize len = si_cstrLen(keywords[i]);
		SI_ASSERT(len <= 8);
		//memcpy(&keywords_U64[i], keywords[i], len);

		//char m[1024];
		//memcpy(m, keywords[i], len + 1);
		//si_cstrUpper(m);
		//si_printf("SILEX_KEYWORD_%s,\n", m, m);
	}

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
	alloc[SC_VARS] = si_allocatorMake(sizeof(scVariable) * SC_MAX_VARS);
	scLexer lex = silex_lexerMake(text, textLen);

	usize bytes = 0;
	for_range (i, 0, SC_ALLOC_LEN) {
		bytes += alloc[i]->maxLen;
	}
	si_printf("%f MB\n", bytes / 1024.f / 1024.f);
	SI_ASSERT(bytes < SI_MEGA(1));

	scFunction scope_GLOBAL;
	scope_GLOBAL.scope = si_arrayMakeReserve(alloc[SC_VARS], sizeof(scVariable), 0);

	scFunction* curFunc = &scope_GLOBAL;
	scFunction* functions = si_arrayMakeReserve(alloc[SC_FUNC], sizeof(siArrayHeader), 128);

	siTimeStamp ts = si_timeStampStart();
	while (silex_lexerTokenGet(&lex)) {
		switch (lex.token) {
			case SILEX_TOKEN_KEYWORD: {
				scType type = sc_typeGet(&lex);

				if (type.size != -1) {
					SI_ASSERT(lex.token == SILEX_TOKEN_IDENTIFIER);
					scString name = lex.value.text;

					b32 res = silex_lexerTokenGet(&lex);
					SI_ASSERT(res && lex.token == SILEX_TOKEN_PUNCTUATOR);

					switch (lex.value.punctuator) {
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
								SI_ASSERT(res && lex.token == SILEX_TOKEN_IDENTIFIER);

								scVariable var;
								var.name = lex.value.text;
								var.type = type;

								si_arrayPush(&func->parameters, var);

								res = silex_lexerTokenGet(&lex);
								SI_ASSERT(res && lex.token == SILEX_TOKEN_PUNCTUATOR);
								siPunctuator punc = lex.value.punctuator;

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
							scVariable* varPtr = si_arrayPush(&curFunc->scope, var);

							scAction action;
							scAction* pAction;

							action.type = SC_ACTION_VAR_MAKE;
							action.value.var = varPtr;
							pAction= si_arrayPush(&curFunc->code, action);

retry:
							res = silex_lexerTokenGet(&lex);
							SI_ASSERT(res);

							switch (lex.token) {
								case SILEX_TOKEN_CONSTANT: {
									pAction->nextActionIndex = si_arrayLen(curFunc->code);

									action.type = SC_ACTION_VAR_ASSIGN_CONSTANT;
									action.nextActionIndex = 0;
									action.value.token = lex.value;
									pAction = si_arrayPush(&curFunc->code, action);
									goto retry;
								}
								case SILEX_TOKEN_PUNCTUATOR: {
									if (lex.value.punctuator == ';') {
										break;
									}
									SI_PANIC();
								}
								case SILEX_TOKEN_OPERATOR: {
									pAction->nextActionIndex = si_arrayLen(curFunc->code);

									res = silex_lexerTokenGet(&lex);
									SI_ASSERT(res);

									switch (lex.token) {
										case SILEX_TOKEN_CONSTANT: action.type = SC_ACTION_VAR_ADD_CONSTANT; break;
										default: SI_PANIC();
									}
									action.nextActionIndex = 0;
									action.value.token = lex.value;
									pAction = si_arrayPush(&curFunc->code, action);

									goto retry;
								}
								default: SI_PANIC();
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

				switch (lex.value.keyword) {
					case SILEX_KEYWORD_RETURN: {

						scAction action;
						action.type = SC_ACTION_RETURN;
						//action.value.token.type = lex.token;
						//action.value.token.value = lex.value;

						b32 res = silex_lexerTokenGet(&lex);
						SI_ASSERT(res && lex.token == SILEX_TOKEN_CONSTANT);

						res = silex_lexerTokenGet(&lex);
						switch (lex.token) {
							case SILEX_TOKEN_PUNCTUATOR: {
								if (lex.value.punctuator == ';') {
									break;
								}
								SI_PANIC();
							}
							default: SI_PANIC();
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
		scAction action = curFunc->code[i];

		si_printf("%llu: %i\n", i, action.type);
	}


	si_timeStampPrintSince(ts);


	for_range (i, 0, countof(alloc)) {
		si_allocatorFree(alloc[i]);
	}
}
