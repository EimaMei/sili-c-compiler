/*
sililex.h - a C89 lexer
===========================================================================
	- YOU MUST DEFINE 'SILEX_IMPLEMENTATION' in EXACTLY _one_ C file that includes
	this header, BEFORE the include like this:

		#define SILEX_IMPLEMENTATION
		#include "sililex.h"

	- All other files should just include the library without the #define macro.


===========================================================================
MACROS
	- For any of the macros to work, you must _always_ define it before including
	the library. Example:
	```c
		#define SILEX_IMPLEMENTATION
		#define SI_RELEASE_MODE
		#include "sililex.h"
	```

	- SILEX_NO_TYPEDEFS - TODO

	- SILEX_NO_STDINT_INCLUDE - TODO

	- SILEX_USE_HASH - TODO

	- SILEX_HASH_TYPE - TODO

	- SILEX_HASH_FUNC_INIT - TODO

	- SILEX_HASH_FUNC - TODO

	- SILEX_HASH_FUNC_END - TODO

	- SILEX_NO_TEXT - TODO

	- SILEX_NO_LEN - TODO



===========================================================================

LICENSE:
	- This software is licensed under the zlib license (see the LICENSE at the
	bottom of the file).

WARNING
	- This library is _slightly_ experimental and features may not work as
	expected.
	- This also means that functions may not be documented.

*/

#ifndef SILEX_INCLUDE_SI_H
#define SILEX_INCLUDE_SI_H

#include <sili.h>

#if defined(__cplusplus)
extern "C" {
#endif


#if !defined (SILEX_UNARY_CHAR_LIMIT)
	#define SILEX_UNARY_CHAR_LIMIT (512)
#endif


#if defined(SILEX_USE_HASH)
	#ifndef SILEX_HASH_TYPE
		#define SILEX_HASH_TYPE u64
	#endif

	typedef SILEX_HASH_TYPE scHashType;
#endif

typedef SI_ENUM(i32, scTokenType) {
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

typedef SI_ENUM(i32, scTokenState) {
	SILEX_STATE_NORMAL,
	SILEX_STATE_WARNING,
	SILEX_STATE_ERROR
};

typedef SI_ENUM(i32, scErrorType) {
	SILEX_ERROR_SUFFIX,
	SILEX_ERROR_END,

	SILEX_WARNING_UNKNOWN_ESC_SEQUENCE,
	SILEX_WARNING_MULTICHAR,
};

typedef SI_ENUM(i32, scKeyword) {
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

	SILEX_KEYWORD___TYPEOF__,

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

typedef SI_ENUM(i32, scConstantType) {
	SILEX_CONSTANT_NUM_SIGNED = 1,
	SILEX_CONSTANT_NUM_UNSIGNED,
	SILEX_CONSTANT_FLOAT,
};

typedef struct {
	scConstantType type;
	union {
		u64 integer;
		f64 floats;
	} value;
} scConstant;

typedef SI_ENUM(i32, scPunctuator) {
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

typedef SI_ENUM(i32, scOperator) {
	SILEX_OPERATOR_PLUS = 1,
	SILEX_OPERATOR_MINUS,
	SILEX_OPERATOR_MULTIPLY,
	SILEX_OPERATOR_DIVIDE,

	SILEX_OPERATOR_PLUS_PLUS,
	SILEX_OPERATOR_MINUS_MINUS,
	SILEX_OPERATOR_TILDE,
	SILEX_OPERATOR_EXCLAMATION_MARK,

	SILEX_OPERATOR_PLUS_ASSIGN,
	SILEX_OPERATOR_MINUS_ASSIGN
};


typedef struct {
#if !defined(SILEX_NO_TEXT)
	cstring text;
#endif

#if !defined(SILEX_NO_LEN)
	usize len;
#endif

#if defined(SILEX_USE_HASH)
	scHashType hash;
#endif
} scString;

typedef union {
	scString identifier;
	scKeyword keyword;
	scPunctuator punctuator;
	scConstant constant;
	scOperator operator;
} scToken;

typedef SI_ENUM(u32, __scTokenNumberBits) {
	SC__STATE_UNSIGNED = SI_BIT(0),
	SC__STATE_LONG = SI_BIT(1),
	SC__STATE_NUM_EXISTS = SI_BIT(2)
};

typedef struct {
	cstring curData;
	cstring end;
	cstring lineStart;

	scTokenType type;
	scToken token;
	scTokenState state;

	i32 line;
	i32 column;

	struct {
		scErrorType type;
		cstring data;
		usize len;
	} error;

	u32 base;
	__scTokenNumberBits __num;
} scLexer;

typedef struct {
	scTokenType type;
	scToken token;
} scTokenStruct;

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
	#define SILEX_HASH_FUNC_END(hash)
#endif


#else
	#define SILEX_HASH_FUNC_INIT(hash)
	#define SILEX_HASH_FUNC(hash, character)
	#define SILEX_HASH_FUNC_END(hash)
#endif


/* */
scLexer silex_lexerMake(cstring content, usize len);
/* */
b32 silex_lexerTokenGet(scLexer* lexer);


/* */
cstring silex_operatorCstr(scOperator op);


#if defined(SILEX_IMPLEMENTATION)

#define SC__UNARY_BIT_MINUS SI_BIT(0)
#define SC__UNARY_BIT_TILDE   SI_BIT(1)
#define SC__UNARY_BIT_EXCLAMATION   (SI_BIT(1) | SI_BIT(0))

force_inline
b32 si_charIsUnary(char x) {
	return x == '-' || x == '!' || x == '~' || x == '+';
}

siIntern
scKeyword silex__tokenIdToKeyword(u64 num, const char* pLetter);

siIntern
b32 silex__tokenizeConstantInt(scLexer* lexer, const char* pLetter, isize* unaryBitLen,
		u64* unary);

siIntern
b32 silex__tokenizeConstantChar(scLexer* lexer, const char* pLetter, isize* pUnaryBitLen,
		u64* unary);

scLexer silex_lexerMake(cstring content, usize len) {
	scLexer lexer = {0};
	lexer.curData = content;
	lexer.lineStart = content;
	lexer.end = content + len;
	lexer.base = 10;
	lexer.line = 1;

	return lexer;
}

siIntern
void silex__errorSet(scLexer* lexer, scErrorType type, const char* pLetter, usize len) {
	lexer->type = SILEX_TOKEN_INVALID;
	lexer->state = SILEX_STATE_ERROR;
	lexer->error.type = type;
	lexer->error.data = pLetter;
	lexer->error.len = len;
}

siIntern
void silex__warningSet(scLexer* lexer, scErrorType type, const char* pLetter, usize len) {
	lexer->state = SILEX_STATE_WARNING;
	lexer->error.type = type;
	lexer->error.data = pLetter;
	lexer->error.len = len;
}



b32 silex_lexerTokenGet(scLexer* lexer) {
	SI_STOPIF(lexer->curData >= lexer->end, lexer->type = SILEX_TOKEN_EOF; return false);

	const char* pLetter = lexer->curData;
	/* NOTE(EimaMei): Kadangi C kalba labai kieta ir tikrai nepernelyg laisva,
	 * yra leidžiama turėti belekiek vienviečių ženklų, kurio pradžia prasideda
	 * nuo pirmosios leksemos ženklo. Kas 2 bitai 'unary'-oje yra reikšmė, nurodanti
	 * negatyvo (0b01, -), bitinės papildomojo (0b10, ~) arba NE (0b11, !) operaciją. */
	isize unaryBitLen = -1;
	u64 unary[(SILEX_UNARY_CHAR_LIMIT) / (sizeof(u64) * 8 / 2)];
	memset(unary, 0, sizeof(unary));

#define SKIP_WHITESPACE(lexer, pLetter) \
	do { \
		switch (*pLetter) { \
			case ' ': case '\t': case '\f': \
				pLetter += 1; \
				continue; \
			case '\n': \
				lexer->line += 1; \
				lexer->lineStart = pLetter; \
				pLetter += 1; \
				continue; \
			case '\r': \
				lexer->line += 1; \
				pLetter += 1; \
				SI_STOPIF(*pLetter == '\n', pLetter += 1); \
				lexer->lineStart = pLetter; \
				continue; \
		} \
		break; \
	} while (true)

start:
	SKIP_WHITESPACE(lexer, pLetter);
	SI_STOPIF(pLetter >= lexer->end, lexer->type = SILEX_TOKEN_EOF; return false);

	if (SI_TO_U16(pLetter) == SI_TO_U16("/*")) {
		pLetter += 2;
back:
		while (*pLetter != '*') {
			pLetter += 1;
			if (*pLetter == '\n') { lexer->line += 1; }
		}
		pLetter += 1;
		SI_STOPIF(*pLetter != '/', pLetter += 1; goto back);
		pLetter += 1;
		goto start;
	}

#define SILEX_UNARY_ADD(unary, unaryBitLen, bit) \
	do { \
		SI_ASSERT(unaryBitLen < (isize)sizeof(unary) * 8); \
		unary[unaryBitLen / 32u] |= (u64)(bit) << ((usize)unaryBitLen % 32u); \
		unaryBitLen += 2; \
	} while(0)

	switch (*pLetter) {
		default: {
			if (si_charIsAlpha(*pLetter) || *pLetter == '_' || *(u8*)pLetter >= 128) {
				SILEX_HASH_FUNC_INIT(hash);
				const char* start = pLetter;

				while (si_charIsAlphanumeric(*pLetter) || *pLetter == '_' || *(u8*)pLetter >= 128) {
					SILEX_HASH_FUNC(hash, *pLetter);
					pLetter += 1;
				}
				lexer->curData = pLetter;
				lexer->column = pLetter - lexer->lineStart;
				lexer->state = SILEX_STATE_NORMAL;

				usize len = pLetter - start;
				if (len <= 10) {
					u64 stringInt = 0;
					memcpy(&stringInt, start, len);

					scKeyword keyword = silex__tokenIdToKeyword(stringInt, pLetter);
					if (keyword != SILEX_KEYWORD_NONE) {
						lexer->type = SILEX_TOKEN_KEYWORD;
						lexer->token.keyword = keyword;
						return true;
					}
				}

				lexer->type = SILEX_TOKEN_IDENTIFIER;
#ifndef SILEX_NO_TEXT
				lexer->token.identifier.text = start;
#endif
#ifndef SILEX_NO_LEN
				lexer->token.identifier.len = len;
#endif
#ifdef SILEX_USE_HASH
				lexer->token.identifier.hash = hash;
#endif
				lexer->__num = SC__STATE_NUM_EXISTS;
				unaryBitLen = -1;

				return true;
			}
			siFallthrough;
		}

		case '!': {
			pLetter += 1;
			SKIP_WHITESPACE(lexer, pLetter);

			if ((lexer->__num & SC__STATE_NUM_EXISTS) == 0) {
				if (unaryBitLen == -1) {
					const char* check = pLetter;
					while (!si_charIsAlphanumeric(*check)) { check += 1; }
					unaryBitLen = -2 * !si_charIsDigit(*check);
				}

				if (unaryBitLen != -2) {
					SILEX_UNARY_ADD(unary, unaryBitLen, SC__UNARY_BIT_EXCLAMATION);
					goto start;
				}
			}
			lexer->curData = pLetter;
			lexer->column = pLetter - lexer->lineStart;
			lexer->state = SILEX_STATE_NORMAL;
			lexer->__num = 0;

			lexer->type = SILEX_TOKEN_OPERATOR;
			lexer->token.operator = SILEX_OPERATOR_EXCLAMATION_MARK;
			return true;
		}


		case '~': {
			pLetter += 1;
			SKIP_WHITESPACE(lexer, pLetter);

			if ((lexer->__num & SC__STATE_NUM_EXISTS) == 0) {
				if (unaryBitLen == -1) {
					const char* check = pLetter;
					while (!si_charIsAlphanumeric(*check)) { check += 1; }
					unaryBitLen = -2 * !si_charIsDigit(*check);
				}

				if (unaryBitLen != -2) {
					SILEX_UNARY_ADD(unary, unaryBitLen, SC__UNARY_BIT_TILDE);
					goto start;
				}
			}
			lexer->curData = pLetter;
			lexer->type = SILEX_TOKEN_OPERATOR;
			lexer->token.operator = SILEX_OPERATOR_TILDE;

			lexer->column = pLetter - lexer->lineStart;
			lexer->state = SILEX_STATE_NORMAL;
			lexer->__num = 0;
			return true;
		}

		case '-': case '+': {
			const char* ogChar = pLetter;
			pLetter += 1;

			if (*pLetter == *ogChar) {
				lexer->curData = pLetter + 1;
				lexer->type = SILEX_TOKEN_OPERATOR;
				lexer->token.operator = SILEX_OPERATOR_PLUS_PLUS + (*ogChar == '-');

				lexer->column = pLetter - lexer->lineStart;
				lexer->state = SILEX_STATE_NORMAL;
				lexer->__num = 0;
				return true;
			}
			else if (*pLetter == '=') {
				lexer->curData = pLetter + 1;
				lexer->type = SILEX_TOKEN_OPERATOR;
				lexer->token.operator = SILEX_OPERATOR_PLUS_ASSIGN + (*ogChar == '-');

				lexer->column = pLetter - lexer->lineStart;
				lexer->state = SILEX_STATE_NORMAL;
				lexer->__num = 0;
				return true;
			}
			SKIP_WHITESPACE(lexer, pLetter);

			b32 isNeg = (*ogChar == '-');
			if ((lexer->__num & SC__STATE_NUM_EXISTS) == 0) {
				if (unaryBitLen == -1) {
					const char* check = pLetter;
					while (!si_charIsAlphanumeric(*check)) { check += 1; }
					unaryBitLen = -2 * !si_charIsDigit(*check);
				}

				if (unaryBitLen != -2) {
					if (isNeg) {
						SILEX_UNARY_ADD(unary, unaryBitLen, SC__UNARY_BIT_MINUS);
					}
					goto start;
				}
			}

			lexer->curData = pLetter;
			lexer->type = SILEX_TOKEN_OPERATOR;
			lexer->token.operator = SILEX_OPERATOR_PLUS + isNeg;

			lexer->column = pLetter - lexer->lineStart;
			lexer->state = SILEX_STATE_NORMAL;
			lexer->__num = 0;

			return true;
		}


		case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7':
		case '8': case '9': {
			return silex__tokenizeConstantInt(lexer, pLetter, &unaryBitLen, unary);
		}

		case 'L': {
			if (pLetter[1] == '\'') {
				pLetter += 1;
				siFallthrough;
			}
			else {
				SI_PANIC();
			}
		}
		case '\'': {
			return silex__tokenizeConstantChar(lexer, pLetter, &unaryBitLen, unary);
		}


		/* TODO(EimaMei): Reikia apdoroti atvejį, kai žvaigždutės simbolis
		yra naudojamas kaip daugybos, o ne skyrybos ženklu. SC__STATE_NUM_EXISTS. */
		case '*':
			lexer->curData = pLetter + 1;

			if (lexer->type == SILEX_TOKEN_KEYWORD) {
				b32 valid = silex_keywordIsType(lexer->token.keyword);
				SI_ASSERT(valid);
			}

			lexer->type = SILEX_TOKEN_PUNCTUATOR;
			lexer->token.punctuator = '*';

			lexer->column = pLetter - lexer->lineStart;
			lexer->state = SILEX_STATE_NORMAL;
			lexer->__num = 0;

			return true;

		case '(': case ')': case '[': case ']': case '{': case '}': case ';': case '=':
		case ',':
			lexer->curData = pLetter + 1;

			lexer->type = SILEX_TOKEN_PUNCTUATOR;
			lexer->token.punctuator = *pLetter;

			lexer->column = pLetter - lexer->lineStart;
			lexer->state = SILEX_STATE_NORMAL;
			lexer->__num = SC__STATE_NUM_EXISTS * (*pLetter == ')');

			return true;
	}
	SI_PANIC();

	return false;
}


cstring silex_operatorCstr(scOperator op) {
    switch (op) {
        case SILEX_OPERATOR_PLUS: return "+";
        case SILEX_OPERATOR_MINUS: return "-";
        case SILEX_OPERATOR_MULTIPLY: return "*";
        case SILEX_OPERATOR_DIVIDE: return "/";
        case SILEX_OPERATOR_PLUS_PLUS: return "++";
        case SILEX_OPERATOR_MINUS_MINUS: return "--";
        case SILEX_OPERATOR_TILDE: return "~";
        case SILEX_OPERATOR_EXCLAMATION_MARK: return "!";
        case SILEX_OPERATOR_PLUS_ASSIGN: return "+=";
        case SILEX_OPERATOR_MINUS_ASSIGN: return "-=";
        default: return "?";
    }
}

static
u64 silex__tokenizeUnary(u64 value, isize* pUnaryBitLen, u64* unary) {
	isize unaryBitLen = *pUnaryBitLen;
	isize i = unaryBitLen;

	while (i > 0) {
		u64* unaryByte = &unary[i / 32u];

		u32 totalBitCount = i % 32u;
		u32 bitCount = totalBitCount / 2u;

		totalBitCount -= 2u;
		u32 bits = (u64)(SI_BIT(0) | SI_BIT(1)) << totalBitCount;
		for_range (j, 0, bitCount) {
			u32 unaryBits = (*unaryByte & bits) >> (totalBitCount - j * 2u);

			switch (unaryBits) {
				case SC__UNARY_BIT_MINUS: value = -value; break;
				case SC__UNARY_BIT_TILDE: value = ~value; break;
				case SC__UNARY_BIT_EXCLAMATION:	  value = !value; break;
				default: SI_PANIC();
			}
			bits >>= 2;
		}
		*unaryByte = 0;
		i -= 32;
	}
	*pUnaryBitLen = -1;

	return value;
}

static
b32 silex__tokenizeConstantInt(scLexer* lexer, const char* pLetter, isize* pUnaryBitLen,
		u64* unary) {
	u64 value = 0;
	b32 running = true;
	u32 base = lexer->base;

	cstring suffixStart = nil;
	b32 suffixInvalid = false;

	while (running) {
		char x = si_charUpper(*pLetter);

		i32 digit = (x - '0');
		if (si_between(digit, 0, base - 1)) {
			value *= base;
			value += digit;
			pLetter += 1;
			continue;
		}
		else if (suffixStart == nil) {
			suffixStart = pLetter;
		}

		switch (x) {
			case 'l': {
				if ((lexer->__num & SC__STATE_LONG) == 0) {
					lexer->__num |= SC__STATE_LONG;
					pLetter += 1;
					continue;
				}
				pLetter += 1;
				suffixInvalid = true;
				running = false;
				break;
			}
			case 'u': {
				if ((lexer->__num & SC__STATE_UNSIGNED) == 0) {
					lexer->__num |= SC__STATE_UNSIGNED;
					pLetter += 1;
					continue;
				}
				pLetter += 1;
				suffixInvalid = true;
				running = false;
				break;
			}
			default: {
				suffixInvalid = si_charIsAlpha(x);
				while (si_charIsAlpha(*pLetter)) { pLetter += 1; }
				running = false;
			}
		}
	}

	lexer->curData = pLetter;
	lexer->type = SILEX_TOKEN_CONSTANT;
	lexer->state = SILEX_STATE_NORMAL;

	value = silex__tokenizeUnary(value, pUnaryBitLen, unary);

	scConstant* constant = &lexer->token.constant;
	constant->value.integer = value;
	constant->type = (lexer->__num & SC__STATE_UNSIGNED)
		? SILEX_CONSTANT_NUM_UNSIGNED
		: SILEX_CONSTANT_NUM_SIGNED;

	lexer->base = 10;
	lexer->column = pLetter - lexer->lineStart;
	lexer->__num = SC__STATE_NUM_EXISTS;

	if (suffixInvalid) {
		silex__errorSet(lexer, SILEX_ERROR_SUFFIX, suffixStart, pLetter - suffixStart);
		return false;
	}

	return true;
}

static
b32 silex__tokenizeConstantChar(scLexer* lexer, const char* pLetter, isize* pUnaryBitLen,
		u64* unary) {
	u64 value = 0;
	b16 invalid = false;
	b16 unknown = false;
	b32 running = true;
	cstring start = pLetter;

	pLetter += 1;
	while (running) {
		char x = *pLetter;

		switch (x) {
			case '\'': running = false; break;
			case '\\': {
				value <<= 8;
				switch (pLetter[1]) {
					case '\'': value |= '\''; break;
					case '"': value |= '\"'; break;
					case '?': value |= '\?'; break;
					case '\\': value |= '\\'; break;
					case 'a': value |= '\a'; break;
					case 'b': value |= '\b'; break;
					case 'f': value |= '\f'; break;
					case 'n': value |= '\n'; break;
					case 'r': value |= '\r'; break;
					case 't': value |= '\t'; break;
					case 'v': value |= '\v'; break;

					case '0': case '1': case '2': case '3': case '4': case 5: case 6: case '7':
						value |= pLetter[1] - '0';
						if (si_between(pLetter[2], '0', '7')) {
							value *= 8;
							value |= pLetter[2] - '0';
							pLetter += 1;
						}
						break;

					case 'x': {
						i32 x = si_charHexDigitToInt(pLetter[2]);
						if (x != -1) {
							value |= x;
							x = si_charHexDigitToInt(pLetter[3]);
							if (x != -1) {
								value *= 16;
								value |= x;
								pLetter += 2;
								break;
							}
							pLetter += 1;

							break;
						}
						invalid = true;
						break;
					}

					default: {
						value |= pLetter[1];
						unknown = true;
					}
				}
				pLetter += 2;

				continue;
			}
			default: {
				value <<= 8;
				value |= x;
			}
		}

		pLetter += 1;
	}

	lexer->curData = pLetter;
	lexer->type = SILEX_TOKEN_CONSTANT;
	lexer->state = SILEX_STATE_NORMAL;

	value = silex__tokenizeUnary(value, pUnaryBitLen, unary);

	scConstant* constant = &lexer->token.constant;
	constant->value.integer = value;
	constant->type = SILEX_CONSTANT_NUM_SIGNED;

	lexer->base = 10;
	lexer->column = pLetter - lexer->lineStart;
	lexer->__num = SC__STATE_NUM_EXISTS;

	if (unknown) {
		silex__warningSet(
			lexer, SILEX_WARNING_UNKNOWN_ESC_SEQUENCE, start, pLetter - 1 - start
		);

	}
	else if (value > 0xFF) {
		silex__warningSet(
			lexer, SILEX_WARNING_MULTICHAR, start, pLetter - 1 - start
		);
	}

	return true;
}



siIntern
scKeyword silex__tokenIdToKeyword(u64 num, cstring pLetter) {

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
#define SILEX__TOKENID___TYPEOF 0x666F657079745F5F
#define SILEX__TOKENID___ 0x5F5F
#endif

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

		case SILEX__TOKENID___TYPEOF: {
			u16 stringInt = 0;
			memcpy(&stringInt, pLetter - 2, 2);
			return stringInt == SILEX__TOKENID___;
		}

	}

	return SILEX_KEYWORD_NONE;
}

#endif

#if defined(__cplusplus)
}
#endif

#endif /* SILEX_INCLUDE_SI_H */


/*
------------------------------------------------------------------------------
Copyright (C) 2024 EimaMei

This software is provided 'as-is', without any express or implied warranty. In
no event will the authors be held liable for any damages arising from the use of
this software.

Permission is granted to anyone to use this software for any purpose, including
commercial applications, and to alter it and redistribute it freely, subject to
the following restrictions:

1. The origin of this software must not be misrepresented; you must not
	 claim that you wrote the original software. If you use this software
	 in a product, an acknowledgment in the product documentation would be
	 appreciated but is not required.
2. Altered source versions must be plainly marked as such, and must not be
	 misrepresented as being the original software.
3. This notice may not be removed or altered from any source distribution.
------------------------------------------------------------------------------
*/
