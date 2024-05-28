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
};


typedef struct {
	u8 __blank : 4;
	u8 w : 1;
	u8 r : 1;
	u8 x : 1;
	u8 b : 1;
} scX86RexPrefix;

#define X86RMBYTE(mod, reg, rm) (((mod) << 6) | ((reg) << 3) | (rm))


#define X86_MOV_R32_RM32 0x8B

#define X86_MOV_RM32_I32 0xC7

typedef SI_ENUM(u8, scX86OperandType) {
	X86_OPERAND_M8 = 1,
	X86_OPERAND_M32,
	X86_OPERAND_REG
};

typedef SI_ENUM(u32, siX86Config) {
	X86_CFG_64BIT = SI_BIT(0),
	X86_CFG_REX_PREFIX = SI_BIT(1),
	SC_X86_CONFIG_ID = SI_BIT(2),
};

force_inline
usize sc_x86Opcode(u8 opcode, siX86Config config, u32 ptr, u32 value, u8* out) {
	usize i = 0;

	if (config & X86_CFG_REX_PREFIX) {
		scX86RexPrefix prefix;
		prefix.__blank = 0x4;
		prefix.w = (config & X86_CFG_64BIT) != 0;
		prefix.x = 0;
		prefix.b = 0;

		out[i] = *(u8*)&prefix;
		i += 1;
	}

	u8 mod, reg, rm;
	if (ptr == 0) {
		mod = X86_OPERAND_REG;
		reg = value;
		rm = value;

		out[i] = opcode, i += 1;
		out[i] = X86RMBYTE(mod, reg, rm), i += 1;
	}
	else {
		mod = ptr <= 255 ? X86_OPERAND_M8 : X86_OPERAND_M32;
		reg = 0;
		rm = RBP;

		out[i] = opcode, i += 1;
		out[i] = X86RMBYTE(mod, reg, rm), i += 1;

		if (mod == X86_OPERAND_M8) {
			u8 val = -ptr;
			memcpy(&out[i], &val, 1), i += 1;
		}
		else {SI_PANIC(); }

		if (value != 0) {
			memcpy(&out[i], &value, 4), i += 4;
		}
	}

	return i;
}
