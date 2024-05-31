#include <sili.h>

typedef SI_ENUM(u32, scX86Register) {
	RAX = 0,
	RCX,
	RDX,
	RBX,
	RSP,
	RBP,
	RSI,
	RDI,

	EAX = 0,
	ECX,
	EDX,
	EBX,
	ESP,
	EBP,
	ESI,
	EDI,

};


#define X86REX(w, r, x, b) ((u8)0x40 | ((u8)(w) << 3) | ((u8)(r) << 2) | ((u8)(x) << 1) | (u8)(b))
#define X86RMBYTE(mod, reg, rm) (((mod) << 6) | ((reg) << 3) | (rm))

#define X86_PUSH_R64 0x50
#define X86_POP_R64 0x58

#define X86_MOV_RM32_R32 0x89
#define X86_MOV_RM64_R64 X86_MOV_RM32_R32

#define X86_MOV_R32_RM32 0x8B
#define X86_MOV_R64_RM64 X86_MOV_R32_RM32

#define X86_MOV_R32_I32 0xB8
#define X86_RET 0xC3
#define X86_MOV_RM32_I32 0xC7

typedef SI_ENUM(u8, scX86OperandType) {
	X86_OPERAND_M8 = 1,
	X86_OPERAND_M32,
	X86_OPERAND_REG
};

typedef SI_ENUM(u32, siX86Config) {
	X86_CFG_64BIT = SI_BIT(0),
	X86_CFG_REX_PREFIX = SI_BIT(1),
	X86_CFG_ID = SI_BIT(2),
	X86_CFG_RMB = SI_BIT(3),

	X86_CFG_DST_R = SI_BIT(4),
	X86_CFG_SRC_R = SI_BIT(5),
	X86_CFG_DST_M = SI_BIT(6),
	X86_CFG_SRC_M = SI_BIT(7),

	X86_CFG_DST_BITS = X86_CFG_DST_R | X86_CFG_DST_M,
	X86_CFG_SRC_BITS = X86_CFG_SRC_R | X86_CFG_SRC_M,
};


force_inline
usize sc_x86Opcode(u8 opcode, siX86Config config, u32 dst, u32 src, u8* out) {
	SI_ASSERT(config == 0 || (config & (X86_CFG_DST_BITS | X86_CFG_SRC_BITS)) != 0);
	SI_ASSERT((config & (X86_CFG_DST_BITS)) != X86_CFG_DST_BITS);
	SI_ASSERT((config & (X86_CFG_SRC_BITS)) != X86_CFG_SRC_BITS);
	usize i = 0;

	if (config & X86_CFG_REX_PREFIX) {
		out[i] = X86REX(((config & X86_CFG_64BIT) != 0), 0, 0, 0);
		i += 1;
	}

	out[i] = opcode, i += 1;

	if (config & X86_CFG_RMB) {
		u8 mod, reg, rm;
		if (config & (X86_CFG_DST_R | X86_CFG_SRC_R)) {
			mod = X86_OPERAND_REG;
		}
		else {
			mod = dst <= 255 ? X86_OPERAND_M8 : X86_OPERAND_M32;
		}

		switch (config & X86_CFG_SRC_BITS) {
			case X86_CFG_SRC_R:
				reg = src;
				break;
			case X86_CFG_SRC_M:
				reg = 0;
				break;
		}

		switch (config & X86_CFG_DST_BITS) {
			case X86_CFG_DST_R:
				rm = dst;
				break;
			case X86_CFG_DST_M:
				rm = RBP;
				break;
		}

		out[i] = X86RMBYTE(mod, reg, rm), i += 1;

		if (mod == X86_OPERAND_M8) {
			u8 val = -src;
			memcpy(&out[i], &val, 1), i += 1;
		}
		else if (mod == X86_OPERAND_M32) {SI_PANIC(); }
	}

	if (config & X86_CFG_ID) {
		memcpy(&out[i], &src, 4), i += 4;
	}
	return i;
}
