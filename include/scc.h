#ifndef SC_SCC_INCLUDE_H
#define SC_SCC_INCLUDE_H

#define SILEX_USE_HASH
#include <sili.h>
#include <sililex.h>


typedef SI_ENUM(i32, scAstNodeType) {
	SC_AST_NODE_TYPE_IDENTIFIER = 1,
	SC_AST_NODE_TYPE_CONSTANT,
	SC_AST_NODE_TYPE_BINARY_OP,
	SC_AST_NODE_TYPE_UNARY_OP,
	SC_AST_NODE_TYPE_GROUP_OP
};

typedef struct scAstNode {
	scAstNodeType type;
	union {
		scString identifier;
		scConstant constant;
		struct {
			struct scAstNode* left;
			struct scAstNode* right;
			scOperator operator;
		} binary;
		struct {
			struct scAstNode* operand;
			scOperator operator;
		} unary;
		struct {
			struct scAstNode* start;
		} group;
	} data;
} scAstNode;

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

typedef SI_ENUM(u32, scActionType) {
	SC_ACTION_SCOPE_BEGIN = 1,
	SC_ACTION_SCOPE_END,

	SC_ACTION_VAR_ASSIGN,
	SC_ACTION_VAR_ADD,
	SC_ACTION_VAR_SUB,
	SC_ACTION_VAR_MUL,
	SC_ACTION_VAR_DIV,

	SC_ACTION_NEG,

	SC_ACTION_RETURN,
};
typedef struct {
	scActionType type;
	scTokenStruct* values;
	scAstNode* root;
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

	cstring fileName;
	scString funcName;
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

typedef SI_ENUM(u32, scAsmType) {
	SC_ASM_FUNC_START = 1,
	SC_ASM_REG_SET,
	SC_ASM_REG_UNSET,

	SC_ASM_PUSH_R64,

	SC_ASM_POP_R8,

	SC_ASM_LD_R8_R8 = SC_ASM_POP_R8 + 4,
	SC_ASM_LD_R8_M8 = SC_ASM_LD_R8_R8 + 4,
	SC_ASM_LD_R64_M64 = SC_ASM_LD_R8_M8 + 4,

	SC_ASM_LD_R8_I8,
	SC_ASM_LD_M8_I8 = SC_ASM_LD_R8_I8 + 4,
	SC_ASM_LD_M8_M8 = SC_ASM_LD_M8_I8 + 4,
	SC_ASM_LD_M8_R8 = SC_ASM_LD_M8_M8 + 4,


	SC_ASM_ARITH_R64_M64 = SC_ASM_LD_M8_R8 + 4,


	SC_ASM_ADD_R8_R8 = SC_ASM_ARITH_R64_M64 + 4,
	SC_ASM_SUB_R8_R8 = SC_ASM_ADD_R8_R8 + 4,

	SC_ASM_ADD_R8_I8 = SC_ASM_SUB_R8_R8 + 4,
	SC_ASM_SUB_R8_I8 = SC_ASM_ADD_R8_I8 + 4,

	SC_ASM_ADD_R8_M8 = SC_ASM_SUB_R8_I8 + 4,
	SC_ASM_SUB_R8_M8 = SC_ASM_ADD_R8_M8 + 4,

	SC_ASM_ADD_M8_R8 = SC_ASM_SUB_R8_M8 + 4,
	SC_ASM_SUB_M8_R8 = SC_ASM_ADD_M8_R8 + 4,

	SC_ASM_ADD_M8_I8 = SC_ASM_SUB_M8_R8 + 4,
	SC_ASM_SUB_M8_I8 = SC_ASM_ADD_M8_I8 + 4,

	SC_ASM_ADD_M8_M8 = SC_ASM_SUB_M8_I8 + 4,
	SC_ASM_SUB_M8_M8 = SC_ASM_ADD_M8_M8 + 4,


	SC_ASM_NEG_R8 = SC_ASM_SUB_M8_M8 + 4,
	SC_ASM_NEG_M8 = SC_ASM_NEG_R8 + 4,

	SC_ASM_NOT_R8 = SC_ASM_NEG_M8 + 4,
	SC_ASM_NOT_M8 = SC_ASM_NOT_R8 + 4,


	SC_ASM_CALL = SC_ASM_NOT_M8 + 4,
	SC_ASM_RET,
	SC_ASM_SYSCALL,

	SC_ASM_INSTRUCTION_LEN
};

typedef SI_ENUM(u32, scAsmRegister) {
	SC_ASM_REG_0 = SC_ASM_INSTRUCTION_LEN,
	SC_ASM_REG_1,
	SC_ASM_REG_2,
	SC_ASM_REG_3,
	SC_ASM_REG_4,
	SC_ASM_REG_5,
	SC_ASM_REG_6,
	SC_ASM_REG_7,
	SC_ASM_REG_8,
	SC_ASM_REG_9,
	SC_ASM_REG_10,
	SC_ASM_REG_11,
	SC_ASM_REG_12,
	SC_ASM_REG_13,
	SC_ASM_REG_14,
	SC_ASM_REG_15,

	SC_ASM_REG_PARAM_0,
	SC_ASM_REG_PARAM_1,
};
#define SC_ASM_REG_RET UINT32_MAX
#define SC_ASM_REG_PARAM_MAX (UINT32_MAX - SC_ASM_INSTRUCTION_LEN - 1)

typedef struct {
	scAsmType type;
	u32 dst;
	u32 src;
} scAsm;
SI_STATIC_ASSERT(sizeof(scAsm) == 12);


typedef struct {
	b16 registers;
	scInfoTable* scope;
} scAsmEnvironmentState;


typedef SI_ENUM(u32, scIndex) {
	SC_FILE,
	SC_SCOPE,
	SC_MAIN,

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

extern scType type_size_t;


#define SC_ALLOCATOR_MAKE(type, limit, ...) \
	do { \
		alloc[(type)] = si_allocatorMake(__VA_ARGS__); \
		usize bytes = alloc[(type)]->maxLen; \
		SI_LOG_FMT("%f MB\n", bytes / 1024.f / 1024.f); \
		SI_ASSERT(bytes <= limit); \
	} while (0)


#define sc_typeCmp(t1, t2) ((t1)->size == (t2)->size && SI_TO_U64((isize*)(t1) + 1) == SI_TO_U64((isize*)(t2) + 1))
#define sc_typeIsVoid(type) ((type)->size == 0)

#define sc_variableErrorCheck(error) \
	do { \
		if (error > 1) { \
			cstring errors[] = {"Variable doesn't exist", "Cannot use type/function as a valid initializer."}; \
			SI_PANIC_MSG(errors[error == 3]); \
		} \
	} while (0)


#define sc_actionIdentifierGet(actionArr) \
	*si_cast(scIdentifierKey**, &((actionArr)[0]));

#define SI_LOG(msg) si_print(msg)
#define SI_LOG_FMT(msg, ...) si_printf(msg, __VA_ARGS__)


/* */
scType* sc_typeGetFromKeyword(scKeyword keyword);

void sc_tokenGet(scLexer* lex);

/* */
scType* sc_typeGet(scLexer* lex, scInfoTable* scope, scKeyword* outKeyword);
/* */
scType sc_typeMake(scLexer* lex, scType* baseType, scKeyword keyword);
/* */
scType sc_typeGetAndMake(scLexer* lex, scInfoTable* scope);


void sc_constantArithmetic(scConstant* constant, scOperator operator, scConstant src);

scPunctuator sc_actionAddValues(scLexer* lex, scInfoTable* scope, scAction* action);

/* */
void sc_astNodeMake(siArray(scAction) action, b32 firstIsIdentifier);

/* */
scVariable* sc_variableGet(scInfoTable* scope, u64 hash, i32* res);
/* */
scVariable* sc_variableGetAndOptimizeToken(scInfoTable* scope, scTokenStruct* token, i32* res);


/* */
b32 sc_asmRegisterAvailable(scAsmEnvironmentState state, scAsmRegister reg);

/* */
scAsmRegister sc_asmRegisterAny(scAsmEnvironmentState* state, b32 set);

/* */
void sc_asmRegisterSet(scAsmEnvironmentState* state, scAsmRegister reg);
/* */
void sc_asmRegisterUnset(scAsmEnvironmentState* state, scAsmRegister reg);


#endif /* SC_SCC_INCLUDE_H */
