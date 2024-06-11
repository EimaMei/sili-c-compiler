#ifndef SC_SCC_INCLUDE_H
#define SC_SCC_INCLUDE_H

#define SILEX_USE_HASH
#include <sili.h>
#include <sililex.h>


typedef SI_ENUM(u32, scInitializerType) {
	SC_INIT_BINARY = 1,
	SC_INIT_CONSTANT,
	SC_INIT_IDENTIFIER
};

typedef struct scInitializer {
	scInitializerType type;
	union {
		struct {
			scOperator operator;
			scTokenStruct* left;
			scTokenStruct* right;
		} binary;
		scConstant constant;
		u64 identifier;
	} value;

	struct scInitializer* next;
} scInitializer;

typedef SI_ENUM(u32, scActionType) {
	SC_ACTION_SCOPE_BEGIN = 1,
	SC_ACTION_SCOPE_END,

	SC_ACTION_VAR_ASSIGN,
	SC_ACTION_VAR_ADD,
	SC_ACTION_VAR_SUB,
	SC_ACTION_VAR_MUL,
	SC_ACTION_VAR_DIV,
	SC_ACTION_RETURN,
};
typedef struct {
	scActionType type;
	scTokenStruct* values;
} scAction;


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
SI_STATIC_ASSERT(sizeof(scType) == 24);

typedef struct {
	scType type;
	scInitializer* init;
	u32 location;
} scVariable;


typedef struct {
	scType type;
	u64 name;
	usize paramLen;
	scType* paramTypes;
	u32* paramVars;
	siArray(scAction) code;
	u32 location;
} scFunction;


typedef SI_ENUM(u16, scIdentifierKeyType) {
	SC_IDENTIFIER_KEY_FUNC = 1,
	SC_IDENTIFIER_KEY_VAR,
	SC_IDENTIFIER_KEY_TYPE
};

typedef struct {
	scIdentifierKeyType type;
	u16 rank;
	char identifier[];
} scIdentifierKey;
SI_STATIC_ASSERT(sizeof(scIdentifierKey) == 4);


typedef SI_ENUM(u32, scAstNodeType) {
	SC_AST_VAR_MAKE = 1,

	SC_AST_VAR_ADD,
	SC_AST_VAR_SUB,
	SC_AST_VAR_MUL,
	SC_AST_VAR_DIV,

	SC_AST_SCOPE_BEGIN,
	SC_AST_SCOPE_END,
	SC_AST_RETURN
};

typedef struct {
	scAstNodeType type;
	scInitializer* init;
	scIdentifierKey* key;
} scAstNode;
SI_STATIC_ASSERT(sizeof(scAstNode) == 24);


/* NOTE(EimaMei): Ši struktūra galioja tik globajai galiojimo sričiai, įprastiniams
 * yra naudojama 'scInfoTable', kadangi juose neleidžiama deklaruoti funkcijas. */
typedef struct scGlobalInfoTable {
	struct scInfoTable* parent;
	siHt(scIdentifierKey) identifiers;

	u32 scopeRank;
	u32 mainFuncID;

	usize varsLen;
	scVariable* vars;

	usize typesLen;
	scVariable* types;

	usize funcsLen;
	scFunction* funcs;
} scGlobalInfoTable;


typedef struct scInfoTable {
	struct scInfoTable* parent;
	siHt(scIdentifierKey) identifiers;

	u32 stack;
	u32 rank;

	usize varsLen;
	scVariable* vars;

	usize typesLen;
	scVariable* types;
} scInfoTable;
SI_STATIC_ASSERT(sizeof(scInfoTable) == 56);
SI_STATIC_ASSERT(sizeof(scGlobalInfoTable) == sizeof(scInfoTable) + sizeof(usize) * 2);

typedef SI_ENUM(u32, scAsmType) {
	SC_ASM_FUNC_START = 1,

	SC_ASM_PUSH_R64,

	SC_ASM_POP_R64,



	SC_ASM_LD_M8_PARAM,
	SC_ASM_LD_M16_PARAM,
	SC_ASM_LD_M32_PARAM,
	SC_ASM_LD_M64_PARAM,

	SC_ASM_LD_R8_I8,
	SC_ASM_LD_R16_I16,
	SC_ASM_LD_R32_I32,
	SC_ASM_LD_R64_I64,

	SC_ASM_LD_R8_M8,
	SC_ASM_LD_R16_M16,
	SC_ASM_LD_R32_M32,
	SC_ASM_LD_R64_M64,

	SC_ASM_LD_M8_I8,
	SC_ASM_LD_M16_I16,
	SC_ASM_LD_M32_I32,
	SC_ASM_LD_M64_I32,
	SC_ASM_LD_M64_I64,

	SC_ASM_LD_M8_M8,
	SC_ASM_LD_M16_M16,
	SC_ASM_LD_M32_M32,
	SC_ASM_LD_M64_M32,



	SC_ASM_ARITH_R64_M64,



	SC_ASM_ADD_R8_I8,
	SC_ASM_ADD_R16_I16,
	SC_ASM_ADD_R32_I32,
	SC_ASM_ADD_R64_I64,

	SC_ASM_SUB_R8_I8,
	SC_ASM_SUB_R16_I16,
	SC_ASM_SUB_R32_I32,
	SC_ASM_SUB_R64_I64,


	SC_ASM_ADD_R8_M8,
	SC_ASM_ADD_R16_M16,
	SC_ASM_ADD_R32_M32,
	SC_ASM_ADD_R64_M64,

	SC_ASM_SUB_R8_M8,
	SC_ASM_SUB_R16_M16,
	SC_ASM_SUB_R32_M32,
	SC_ASM_SUB_R64_M64,


	SC_ASM_ADD_M8_I8,
	SC_ASM_ADD_M16_I16,
	SC_ASM_ADD_M32_I32,
	SC_ASM_ADD_M64_I64,

	SC_ASM_SUB_M8_I8,
	SC_ASM_SUB_M16_I16,
	SC_ASM_SUB_M32_I32,
	SC_ASM_SUB_M64_I64,


	SC_ASM_ADD_M8_M8,
	SC_ASM_ADD_M16_M16,
	SC_ASM_ADD_M32_M32,
	SC_ASM_ADD_M64_M64,

	SC_ASM_SUB_M8_M8,
	SC_ASM_SUB_M16_M16,
	SC_ASM_SUB_M32_M32,
	SC_ASM_SUB_M64_M64,



	SC_ASM_CALL,

	SC_ASM_RET_R8,
	SC_ASM_RET_R16,
	SC_ASM_RET_R32,
	SC_ASM_RET_R64,

	SC_ASM_RET_I8,
	SC_ASM_RET_I16,
	SC_ASM_RET_I32,
	SC_ASM_RET_I64,

	SC_ASM_RET_M8,
	SC_ASM_RET_M16,
	SC_ASM_RET_M32,
	SC_ASM_RET_M64,


	SC_ASM_SYSCALL,
};

typedef struct {
	scAsmType type : 24;
	u8 typeInfo : 8;
	u32 dst;
	u32 src;
} scAsm;
SI_STATIC_ASSERT(sizeof(scAsm) == 12);

typedef SI_ENUM(u32, scIndex) {
	SC_FILE,
	SC_SCOPE,
	SC_MAIN,

	SC_AST,
	SC_ASM,
	SC_X86ASM,
	SC_EXE,

	SC_ALLOC_LEN
};
extern siAllocator* alloc[SC_ALLOC_LEN];

extern scGlobalInfoTable global_scope;

extern usize sizeof_SIZE_T;

extern scType type_char;
extern scType type_short;
extern scType type_int;
extern scType type_long;
extern scType type_unsigned;
extern scType type_float;
extern scType type_double;


#define SC_ALLOCATOR_MAKE(type, limit, ...) \
	do { \
		alloc[(type)] = si_allocatorMake(__VA_ARGS__); \
		usize bytes = alloc[(type)]->maxLen; \
		si_printf("%f MB\n", bytes / 1024.f / 1024.f); \
		SI_ASSERT(bytes <= limit); \
	} while (0)

#define SC_RETURN_REGISTER 0

#define sc_typeCmp(t1, t2) ((t1)->size == (t2)->size && SI_TO_U64((isize*)(t1) + 1) == SI_TO_U64((isize*)(t2) + 1))
#define sc_typeIsVoid(type) ((type)->size == 0)

#define sc_variableErrorCheck(error) \
	do { \
		if (error > 1) { \
			cstring errors[] = {"Variable doesn't exist", "Cannot use type/function as a valid initializer."}; \
			SI_PANIC_MSG(errors[error == 3]); \
		} \
	} while (0)


#define SI_LOG(msg) si_print(msg)
#define SI_LOG_FMT(msg, ...) si_printf(msg, __VA_ARGS__)



scType* sc_typeGetFromKeyword(scKeyword keyword);

/* */
scType* sc_typeGet(scLexer* lex, scInfoTable* scope, scKeyword* outKeyword);
/* */
scType sc_typeMake(scLexer* lex, scType* baseType, scKeyword keyword);
/* */
scType sc_typeGetAndMake(scLexer* lex, scInfoTable* scope);


void sc_constantArithmetic(scConstant* constant, scOperator operator, scConstant src);

scPunctuator sc_actionAddValues(scLexer* lex, scAction* action);

/* */
#define sc_actionEvaluate(action, node) sc_actionEvaluateEx(action, node, 0)
void sc_actionEvaluateEx(scAction* action, scAstNode* node, usize i);

/* */
scVariable* sc_variableGet(scInfoTable* scope, u64 hash, i32* res);
/* */
scVariable* sc_variableGetAndOptimizeToken(scInfoTable* scope, scTokenStruct* token, i32* res);


#endif /* SC_SCC_INCLUDE_H */
