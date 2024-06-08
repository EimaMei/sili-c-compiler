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

#if defined(SILEX_USE_HASH)
	#ifndef SILEX_HASH_TYPE
		#define SILEX_HASH_TYPE u64
	#endif

	typedef SILEX_HASH_TYPE scHashType;
#endif

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

typedef SI_ENUM(u32, scTokenError) {
	SILEX_ERROR_NONE,
	SILEX_ERROR_SUFFIX_LONG,
	SILEX_ERROR_SUFFIX_UNSIGNED,
	SILEX_ERROR_PREFIX_MINUS,
};

typedef SI_ENUM(u32, scKeyword) {
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

typedef SI_ENUM(u32, scConstantType) {
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

typedef SI_ENUM(u32, scPunctuator) {
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

typedef SI_ENUM(u32, scOperator) {
	SILEX_OPERATOR_PLUS = 1,
	SILEX_OPERATOR_MINUS,
	SILEX_OPERATOR_MULTIPLY,
	SILEX_OPERATOR_DIVIDE,

	SILEX_OPERATOR_PLUSPLUS,
	SILEX_OPERATOR_MINUSMINUS,
};


typedef struct {
#if !defined(SILEX_USE_HASH)
	cstring text;
#else
	scHashType hash;
#endif
#if !defined(SILEX_NO_LEN)
	usize len;
#endif
} scString;

typedef union {
	scString identifier;
	scKeyword keyword;
	scPunctuator punctuator;
	scConstant constant;
	scOperator operator;
} scToken;

typedef SI_ENUM(u32, __scTokenState) {
	SC__STATE_UNSIGNED = SI_BIT(0),
	SC__STATE_LONG = SI_BIT(1),
	SC__STATE_NEGATIVE = SI_BIT(2),
	SC__STATE_NUM_EXISTS = SI_BIT(3),
};

typedef struct {
	cstring curData;
	cstring end;

	scTokenType type;
	scToken token;

	scTokenError error;
	__scTokenState __state;
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



#if defined(SILEX_IMPLEMENTATION)


force_inline
scKeyword silex__tokenIdToKeyword(u64 num) {

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
	SI_STOPIF(lexer->curData >= lexer->end, lexer->type = SILEX_TOKEN_EOF; return false);

	const char* pLetter = lexer->curData;
	while (si_charIsSpace(*pLetter)) { pLetter += 1; }
	SI_STOPIF(pLetter >= lexer->end, lexer->type = SILEX_TOKEN_EOF; return false);

	if (SI_TO_U16(pLetter) == SI_TO_U16("/*")) {
		pLetter += 2;
		u16 end = SI_TO_U16("*/");
		while (SI_TO_U16(pLetter) != end) {
			pLetter += 1;
		}
		pLetter += 2;
	}


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

					scKeyword keyword = silex__tokenIdToKeyword(stringInt);
					if (keyword != SILEX_KEYWORD_NONE) {
						lexer->type = SILEX_TOKEN_KEYWORD;
						lexer->token.keyword = keyword;
						return true;
					}
				}

				lexer->__state = SC__STATE_NUM_EXISTS;
				lexer->type = SILEX_TOKEN_IDENTIFIER;
#ifndef SILEX_NO_LEN
				lexer->token.identifier.len = len;
#endif
#ifndef SILEX_USE_HASH
				lexer->token.identifier.text = start;
#else
				lexer->token.identifier.hash = hash;
#endif

				return true;
			}
			siFallthrough;
		}

		case '-': case '+': {
			const char* ogChar = pLetter;
			pLetter += 1;

			if (*pLetter == *ogChar) {
				lexer->curData = pLetter + 1;
				lexer->type = SILEX_TOKEN_OPERATOR;
				lexer->token.operator = SILEX_OPERATOR_PLUSPLUS + (*ogChar == '-');
				return true;
			}
			while (si_charIsSpace(*pLetter)) { pLetter += 1; }

			if ((lexer->__state & SC__STATE_NUM_EXISTS) == 0 && si_charIsAlphanumeric(*pLetter)) {
				lexer->__state |= SC__STATE_NUM_EXISTS
								| (SC__STATE_NEGATIVE * (*ogChar == '-'));
				goto num;
			}

			lexer->__state = 0;
			lexer->curData = pLetter;
			lexer->type = SILEX_TOKEN_OPERATOR;
			lexer->token.operator = SILEX_OPERATOR_PLUS + (*ogChar == '-');
			return true;
		}


		case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7':
		case '8': case '9': {
			lexer->__state = SC__STATE_NUM_EXISTS;
			u64 value;
			u32 base;
num:
			value = 0;
			base = 10;
			b32 running = true;

			while (running) {
				char x = si_charLower(*pLetter);

				i32 digit = (x - '0');
				if (si_between(digit, 0, base - 1)) {
					value *= base;
					value += digit;
					pLetter += 1;
					continue;
				}

				b32 hasSpace = si_charIsSpace(*pLetter);
				if (hasSpace) {
					pLetter += 1;
					while (si_charIsSpace(*pLetter)) { pLetter += 1; }
					x = *pLetter;
				}

				switch (x) {
					case 'l': {
						if (!hasSpace && (lexer->__state & SI_BIT(1)) == 0) {
							lexer->__state |= SI_BIT(1);
							pLetter += 1;
							continue;
						}

						lexer->curData = pLetter;
						lexer->type = SILEX_TOKEN_INVALID;
						lexer->error = SILEX_ERROR_SUFFIX_LONG;
						return false;
					}
					case 'u': {
						if (!hasSpace && (lexer->__state & SI_BIT(0)) == 0) {
							lexer->__state |= SI_BIT(0);
							pLetter += 1;
							continue;
						}

						lexer->curData = pLetter;
						lexer->type = SILEX_TOKEN_INVALID;
						lexer->error = SILEX_ERROR_SUFFIX_UNSIGNED;
						return false;
					}
					default: {
						running = false;
						break;
					}
				}
			}
			lexer->curData = pLetter;
			lexer->type = SILEX_TOKEN_CONSTANT;

			scConstant* constant = &lexer->token.constant;
			if (lexer->__state & SI_BIT(2)) {
				value = -value;
			}

			constant->value.integer = value;
			if (lexer->__state & SI_BIT(0)) {
				constant->type = SILEX_CONSTANT_NUM_UNSIGNED;
			}
			else {
				constant->type = SILEX_CONSTANT_NUM_SIGNED;
			}

			return true;
		}

		case '*':
			lexer->curData = pLetter + 1;

			if (lexer->type == SILEX_TOKEN_KEYWORD) {
				b32 valid = silex_keywordIsType(lexer->token.keyword);
				SI_ASSERT(valid);

				lexer->__state = 0;
				lexer->type = SILEX_TOKEN_PUNCTUATOR;
				lexer->token.punctuator = '*';
			}

			return true;

		case '(': case ')': case '[': case ']': case '{': case '}': case ';': case '=':
		case ',':
			lexer->curData = pLetter + 1;

			lexer->__state = 0;
			lexer->type = SILEX_TOKEN_PUNCTUATOR;
			lexer->token.punctuator = *pLetter;
			return true;
	}
	SI_PANIC();

	return false;
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
