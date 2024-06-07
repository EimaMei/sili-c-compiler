#include <sili.h>

typedef SI_ENUM(u32, x86Register) {
	RAX = 0,
	RCX,
	RDX,
	RBX,
	RSP,
	RBP,
	RSI,
	RDI,
	R8,
	R9,
	R10,
	R11,
	R12,
	R13,
	R14,
	R15,

	EAX = 0,
	ECX,
	EDX,
	EBX,
	ESP,
	EBP,
	ESI,
	EDI,

};

typedef SI_ENUM(u32, x86CallingConvention) {
	X86_CALLING_CONV_C = 1,
	X86_CALLING_CONV_SYSTEM_V_I386,
	X86_CALLING_CONV_SYSTEM_V_X86,
	X86_CALLING_CONV_MICROSOFT,
};

typedef struct {
	x86CallingConvention conv;
	b16 registers;
} x86EnvironmentState;


#define X86REX(w, r, x, b) ((u8)0x40 | ((u8)(w) << 3) | ((u8)(r) << 2) | ((u8)(x) << 1) | (u8)(b))
#define X86RMBYTE(mod, reg, rm) (((mod) << 6) | ((reg) << 3) | (rm))
#define X86SIBBYTE(scale, index, base) X86RMBYTE(scale, index, base)

#define X86_PUSH_R64 0x50
#define X86_POP_R64 0x58

#define X86_MOV_RM32_R32 0x89
#define X86_MOV_RM64_R64 X86_MOV_RM32_R32

#define X86_ADD_RM32_I32 0x81
#define X86_ADD_RM64_I32 X86_ADD_RM32_I32

#define X86_MOV_R32_RM32 0x8B
#define X86_MOV_R64_RM64 X86_MOV_R32_RM32

#define X86_LEA_R32_RM32 0x8D
#define X86_LEA_R64_RM64 X86_LEA_R32_RM32


#define X86_MOV_R32_I32 0xB8
#define X86_RET 0xC3
#define X86_MOV_RM32_I32 0xC7

#define X86_CALL_REL32 0xE8

#define X86_SYSCALL 0x0F05
#define X86_SYSCALL_H 0x0F
#define X86_SYSCALL_L 0x05

typedef SI_ENUM(u8, x86OperandType) {
	X86_OPERAND_M8 = 1,
	X86_OPERAND_M32,
	X86_OPERAND_REG
};

typedef SI_ENUM(u32, x86Config) {
	X86_CFG_64BIT = SI_BIT(0),
	X86_CFG_REX_PREFIX = SI_BIT(1),
	X86_CFG_ID = SI_BIT(2),
	X86_CFG_RMB = SI_BIT(3),
	X86_CFG_SIB = SI_BIT(4),

	X86_CFG_DST_R = SI_BIT(5),
	X86_CFG_SRC_R = SI_BIT(6),
	X86_CFG_DST_M = SI_BIT(7),
	X86_CFG_SRC_M = SI_BIT(8),

	X86_CFG_SRC_M_NOT_NEG = SI_BIT(9),

	X86_CFG_DST_BITS = X86_CFG_DST_R | X86_CFG_DST_M | X86_CFG_SIB,
	X86_CFG_SRC_BITS = X86_CFG_SRC_R | X86_CFG_SRC_M | X86_CFG_SIB,
};
usize sc_x86OpcodeEx(u8 opcode, x86Config config, u32 dst, u32 src, u8 sibReg, u8* out);

force_inline
usize sc_x86Opcode(u8 opcode, x86Config config, u32 dst, u32 src, u8* out) {
	return sc_x86OpcodeEx(opcode, config, dst, src, 0, out);
}

usize sc_x86OpcodeEx(u8 opcode, x86Config config, u32 dst, u32 src, u8 sibReg, u8* out) {
	SI_ASSERT(config == 0 || (config & (X86_CFG_DST_BITS | X86_CFG_SRC_BITS | X86_CFG_ID)) != 0);
	SI_ASSERT((config & (X86_CFG_DST_BITS)) != X86_CFG_DST_BITS);
	SI_ASSERT((config & (X86_CFG_SRC_BITS)) != X86_CFG_SRC_BITS);
	usize i = 0;

	if (config & (X86_CFG_REX_PREFIX | X86_CFG_64BIT)) {
		out[i] = X86REX(((config & X86_CFG_64BIT) != 0), 0, 0, 0);
		i += 1;
	}

	out[i] = opcode, i += 1;

	if (config & X86_CFG_RMB) {
		u8 mod, reg, rm;
		if ((config & X86_CFG_DST_R) && ((config & (X86_CFG_SRC_M | X86_CFG_SIB)) == 0)) {
			mod = X86_OPERAND_REG;
		}
		else if (config & (X86_CFG_SRC_M | X86_CFG_DST_M)) {
			mod = dst <= UINT8_MAX ? X86_OPERAND_M8 : X86_OPERAND_M32;
		}
		else {
			mod = 0;
		}

		switch (config & X86_CFG_SRC_BITS) {
			case X86_CFG_SRC_R:
				reg = src;
				break;

			case X86_CFG_SIB:
			case X86_CFG_SRC_M:
				reg = dst;
				break;

			case X86_CFG_SIB | X86_CFG_SRC_M:
				reg = dst;
				break;
		}

		switch (config & X86_CFG_DST_BITS) {
			case X86_CFG_DST_R:
				rm = (config & X86_CFG_SRC_M) ? RBP : dst;
				break;
			case X86_CFG_DST_M:
				rm = RBP;
				break;
			case X86_CFG_SIB:
				rm = sibReg;
				break;
		}

		out[i] = X86RMBYTE(mod, reg, rm), i += 1;

		if (config & X86_CFG_SIB) {
			u8 scale, index, base;
			scale = 0;
			index = 4;
			base = sibReg;

			out[i] = X86SIBBYTE(scale, index, base), i += 1;
		}

		if (mod == X86_OPERAND_M8) {
			u8 val = (config & X86_CFG_SRC_M) ? (u8)src : (u8)dst;
			if ((config & X86_CFG_SRC_M_NOT_NEG) == 0) {
				val = -val;
			}
			memcpy(&out[i], &val, 1), i += 1;
		}
		else if (mod == X86_OPERAND_M32) { SI_PANIC(); }

	}

	if (config & X86_CFG_ID) {
		memcpy(&out[i], &src, sizeof(u32)), i += 4;
	}
	return i;
}


x86Register sc_x86PickFunctionArg(x86EnvironmentState* state) {
	switch (state->conv) {
		case X86_CALLING_CONV_SYSTEM_V_X86: {
			u32 regs[] = {RDI, RSI, RDX, RCX, R8, R9};
			for_range (i, 0, countof(regs)) {
				u32 reg = SI_BIT(regs[i]);

				if ((state->registers & reg) == 0) {
					state->registers |= reg;
					return regs[i];
				}
			}
			/* TODO(EimaMei): Suprogramuoti atvėjį, kai funkcijos parametrų ilgis
			 * viršija daugiau negu 6, t. y., kai likusieji parametrai yra rietuvėje.*/
			SI_PANIC();
		}
		default: SI_PANIC();
	}
}

x86Register sc_x86PickAvailableReg(x86EnvironmentState* state) {
	switch (state->conv) {
		case X86_CALLING_CONV_SYSTEM_V_X86: {
			for_range (i, 0, 16) {
				u32 reg = SI_BIT(i);

				if ((state->registers & reg) == 0) {
					return i;
				}
			}
		}
		default: SI_PANIC();
	}

	return UINT32_MAX;
}

