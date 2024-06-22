#include <sili.h>
#include <scc.h>


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
};

typedef SI_ENUM(u32, x86CallingConvention) {
	X86_CALLING_CONV_C = 1,
	X86_CALLING_CONV_SYSTEM_V_I386,
	X86_CALLING_CONV_SYSTEM_V_X86,
	X86_CALLING_CONV_MICROSOFT,
};

typedef struct {
	scAsmEnvironmentState root;
	u8* data;
	usize len;
	x86CallingConvention conv;
} x86EnvironmentState;


#define X86REX(w, r, x, b) ((u8)0x40 | ((u8)(w) << 3) | ((u8)(r) << 2) | ((u8)(x) << 1) | (u8)(b))
#define X86RMBYTE(mod, reg, rm) (((u8)(mod) << 6) | ((u8)(reg) << 3) | (u8)(rm))
#define X86SIBBYTE(scale, index, base) X86RMBYTE(scale, index, base)


#define X86_ADD_RM8_R8 0x00
#define X86_ADD_R8_RM8 0x02

#define X86_SUB_RM8_R8 0x28
#define X86_SUB_R8_RM8 0x2A

#define X86_PUSH_R64 0x50
#define X86_POP_R64 0x58

#define X86_ADD_RM8_I8 0x80
#define X86_SUB_RM8_I8 0x80

#define X86_MOV_RM8_R8 0x88
#define X86_MOV_R8_RM8 0x8A
#define X86_MOV_R32_RM32 0x8B
#define X86_LEA_R32_RM32 0x8D

#define X86_MOV_R32_I32 0xB8

#define X86_RET 0xC3
#define X86_MOV_RM32_I32 0xC7
#define X86_MOV_RM8_I8 0xC6

#define X86_CALL_REL32 0xE8

#define X86_NEG_RM8 0xF6
#define X86_NOT_RM8 0xF6

#define X86_SYSCALL 0x0F05


typedef SI_ENUM(u8, x86ModRM_MOD) {
	X86_MOD_RM_MOD_M8 = 1,
	X86_MOD_RM_MOD_M32,
	X86_MOD_RM_MOD_REG
};

typedef SI_ENUM(u32, x86Config) {
	X86_CFG_64BIT = SI_BIT(0),
	X86_CFG_REX_PREFIX = SI_BIT(1),
	X86_CFG_RMB = SI_BIT(2),
	X86_CFG_SIB = SI_BIT(3),

	X86_CFG_DST_R = SI_BIT(4),
	X86_CFG_SRC_R = SI_BIT(5),
	X86_CFG_DST_M = SI_BIT(6),
	X86_CFG_SRC_M = SI_BIT(7),

	X86_CFG_SRC_M_NOT_NEG = SI_BIT(8),

	X86_CFG_IB = SI_BIT(9),
	X86_CFG_IW = SI_BIT(10),
	X86_CFG_ID = SI_BIT(11),
	X86_CFG_IQ = SI_BIT(12),

	X86_CFG_ADD = SI_BIT(13),
	X86_CFG_OR  = SI_BIT(14),
	X86_CFG_ADC = SI_BIT(15),
	X86_CFG_SBB = SI_BIT(16),
	X86_CFG_AND = SI_BIT(17),
	X86_CFG_SUB = SI_BIT(18),
	X86_CFG_XOR = SI_BIT(19),
	X86_CFG_CMP = SI_BIT(20),

	X86_CFG_NOTATION_2 = SI_BIT(21),
	X86_CFG_NOTATION_3 = SI_BIT(22),


	X86_CFG_INTWORD_BITS = X86_CFG_IB | X86_CFG_IW | X86_CFG_ID,
	X86_CFG_MUL_OP_BITS = X86_CFG_ADD | X86_CFG_OR | X86_CFG_ADC | X86_CFG_SBB | X86_CFG_AND | X86_CFG_SUB | X86_CFG_XOR | X86_CFG_CMP,

	X86_CFG_NOTATION_BITS = X86_CFG_NOTATION_2 | X86_CFG_NOTATION_3,

	X86_CFG_DST_BITS = X86_CFG_DST_R | X86_CFG_DST_M | X86_CFG_SIB,
	X86_CFG_SRC_BITS = X86_CFG_SRC_R | X86_CFG_SRC_M | X86_CFG_SIB | X86_CFG_NOTATION_BITS,
};


#define sc_x86OpcodePush(state, instr) \
	do { \
		u8* out = &(state)->data[(state)->len]; \
		out[0] = (instr); \
		(state)->len += 1; \
		SI_LOG_FMT("%ll02X\n", out[0]); \
	} while (0)

#define sc_x86OpcodePush2(state, instr) \
	do { \
		u8* out = &(state)->data[(state)->len]; \
		out[0] = ((instr) & 0xFF00) >> 8; \
		out[1] = (instr) & 0x00FF; \
		(state)->len += 2; \
		SI_LOG_FMT("%ll02X %ll02X\n", out[0], out[1]); \
	} while (0)



void sc_x86OpcodeEx(x86EnvironmentState* state,  u8 opcode, u32 dst, u32 src, u8 sibReg,
	x86Config config);

x86Register sc_x86PickAvailableReg(x86EnvironmentState* state);
x86Register sc_x86RegisterConvert(x86EnvironmentState* x86, scAsmRegister reg);

force_inline
void sc_x86Opcode(x86EnvironmentState* state, u8 opcode, u32 dst, u32 src, x86Config config) {
	sc_x86OpcodeEx(state, opcode, dst, src, 0, config);
}


void sc_x86Opcode__REG_REG_RMB(x86EnvironmentState* x86, u8 opcode, scAsm* asm,
		x86Config config) {
	usize i = 0;
	u8* out = &x86->data[x86->len];

	x86Register dst = sc_x86RegisterConvert(x86, asm->dst),
				src = sc_x86RegisterConvert(x86, asm->src);

	if (config & X86_CFG_64BIT) {
		out[i] = X86REX(((config & X86_CFG_64BIT) != 0), 0, 0, 0), i += 1;
	}

	out[i] = opcode, i += 1;
	out[i] = X86RMBYTE(X86_MOD_RM_MOD_REG, dst, src), i += 1;
	x86->len += i;

	for_range (j, 0, i) { SI_LOG_FMT("%ll02X ", out[j]); }
	SI_LOG("\n");
}
void sc_x86Opcode__REG_RMB_REG(x86EnvironmentState* x86, u8 opcode, scAsm* asm,
		x86Config config) {
	usize i = 0;
	u8* out = &x86->data[x86->len];

	x86Register dst = sc_x86RegisterConvert(x86, asm->dst),
				src = sc_x86RegisterConvert(x86, asm->src);

	if (config & X86_CFG_64BIT) {
		out[i] = X86REX(((config & X86_CFG_64BIT) != 0), 0, 0, 0), i += 1;
	}

	out[i] = opcode, i += 1;
	out[i] = X86RMBYTE(X86_MOD_RM_MOD_REG, src, dst), i += 1;
	x86->len += i;

	for_range (j, 0, i) { SI_LOG_FMT("%ll02X ", out[j]); }
	SI_LOG("\n");
}

void sc_x86Opcode__REG_RMB(x86EnvironmentState* x86, u8 opcode, scAsm* asm,
		x86Config config) {
	usize i = 0;
	u8* out = &x86->data[x86->len];

	x86Register dst = sc_x86RegisterConvert(x86, asm->dst);

	if (config & X86_CFG_64BIT) {
		out[i] = X86REX(((config & X86_CFG_64BIT) != 0), 0, 0, 0), i += 1;
	}

	u8 reg;
	switch (config & X86_CFG_NOTATION_BITS) {
		case X86_CFG_NOTATION_2: reg = 2; break;
		case X86_CFG_NOTATION_3: reg = 3; break;
		default: SI_PANIC();
	}


	out[i] = opcode, i += 1;
	out[i] = X86RMBYTE(X86_MOD_RM_MOD_REG, reg, dst), i += 1;
	x86->len += i;

	for_range (j, 0, i) { SI_LOG_FMT("%ll02X ", out[j]); }
	SI_LOG("\n");
}

void sc_x86Opcode__MEM_RMB(x86EnvironmentState* x86, u8 opcode, scAsm* asm,
		x86Config config) {
	usize i = 0;
	u8* out = &x86->data[x86->len];

	x86ModRM_MOD mod = asm->dst <= UINT8_MAX ? X86_MOD_RM_MOD_M8 : X86_MOD_RM_MOD_M32;

	if (config & X86_CFG_64BIT) {
		out[i] = X86REX(((config & X86_CFG_64BIT) != 0), 0, 0, 0), i += 1;
	}
	u32 reg;
	switch (config & X86_CFG_NOTATION_BITS) {
		case X86_CFG_NOTATION_2: reg = 2; break;
		case X86_CFG_NOTATION_3: reg = 3; break;
		default: SI_PANIC();
	}


	out[i] = opcode, i += 1;
	out[i] = X86RMBYTE(mod, reg, RBP), i += 1;


	if (mod == X86_MOD_RM_MOD_M8) {
		u8 val = asm->src;
		if ((config & X86_CFG_SRC_M_NOT_NEG) == 0) {
			val = -val;
		}
		memcpy(&out[i], &val, 1), i += 1;
	}
	else if (mod == X86_MOD_RM_MOD_M32) {
		SI_PANIC();
		u32 val = asm->src;
		if ((config & X86_CFG_SRC_M_NOT_NEG) == 0) {
			val = -val;
		}
		memcpy(&out[i], &val, 4), i += 4;
	}

	for_range (j, 0, i) { SI_LOG_FMT("%ll02X ", out[j]); }
	SI_LOG("\n");
}



void sc_x86Opcode__REG_RMB_INT(x86EnvironmentState* x86, u8 opcode, scAsm* asm,
		x86Config config) {
	usize i = 0;
	u8* out = &x86->data[x86->len];

	x86Register dst = sc_x86RegisterConvert(x86, asm->dst);

	if (config & X86_CFG_64BIT) {
		out[i] = X86REX(((config & X86_CFG_64BIT) != 0), 0, 0, 0), i += 1;
	}

	u32 reg;
	switch (config & X86_CFG_MUL_OP_BITS) {
		case X86_CFG_ADD: reg = 0; break;
		case X86_CFG_SUB: reg = 5; break;
		default: SI_PANIC();
	}

	out[i] = opcode, i += 1;
	out[i] = X86RMBYTE(X86_MOD_RM_MOD_REG, reg, dst), i += 1;

	u32 src = asm->src;
	switch (config & X86_CFG_INTWORD_BITS) {
		case 0: break;
		case X86_CFG_IB: memcpy(&out[i], &src, sizeof(u8)), i += 1; break;
		case X86_CFG_IW: memcpy(&out[i], &src, sizeof(u16)), i += 2; break;
		case X86_CFG_ID: memcpy(&out[i], &src, sizeof(u32)), i += 4; break;
		default: SI_PANIC();
	}

	x86->len += i;

	for_range (j, 0, i) { SI_LOG_FMT("%ll02X ", out[j]); }
	SI_LOG("\n");
}


void sc_x86Opcode__REG_MEM_RMB(x86EnvironmentState* x86, u8 opcode, scAsm* asm,
		x86Config config) {
	usize i = 0;
	u8* out = &x86->data[x86->len];

	x86Register dst = sc_x86RegisterConvert(x86, asm->dst);
	x86ModRM_MOD mod = asm->src <= UINT8_MAX ? X86_MOD_RM_MOD_M8 : X86_MOD_RM_MOD_M32;

	if (config & X86_CFG_64BIT) {
		out[i] = X86REX(((config & X86_CFG_64BIT) != 0), 0, 0, 0), i += 1;
	}

	out[i] = opcode, i += 1;
	out[i] = X86RMBYTE(mod, dst, RBP), i += 1;


	if (mod == X86_MOD_RM_MOD_M8) {
		u8 val = asm->src;
		if ((config & X86_CFG_SRC_M_NOT_NEG) == 0) {
			val = -val;
		}
		memcpy(&out[i], &val, 1), i += 1;
	}
	else if (mod == X86_MOD_RM_MOD_M32) {
		SI_PANIC();
		u32 val = asm->src;
		if ((config & X86_CFG_SRC_M_NOT_NEG) == 0) {
			val = -val;
		}
		memcpy(&out[i], &val, 4), i += 4;
	}


	x86->len += i;

	for_range (j, 0, i) { SI_LOG_FMT("%ll02X ", out[j]); }
	SI_LOG("\n");
}


void sc_x86OpcodeEx(x86EnvironmentState* state, u8 opcode, u32 dst, u32 src, u8 sibReg,
		x86Config config) {
	usize i = 0;
	u8* out = &state->data[state->len];

	if (config & (X86_CFG_REX_PREFIX | X86_CFG_64BIT)) {
		out[i] = X86REX(((config & X86_CFG_64BIT) != 0), 0, 0, 0);
		i += 1;
	}

	out[i] = opcode, i += 1;

	if (config & X86_CFG_RMB) {
		u8 mod = 0, reg = 0, rm = 0;
		if ((config & X86_CFG_DST_R) && ((config & (X86_CFG_SRC_M | X86_CFG_SIB)) == 0)) {
			mod = X86_MOD_RM_MOD_REG;
		}
		else if (config & (X86_CFG_SRC_M | X86_CFG_DST_M)) {
			mod = dst <= UINT8_MAX ? X86_MOD_RM_MOD_M8 : X86_MOD_RM_MOD_M32;
		}

		if (config & X86_CFG_MUL_OP_BITS) {
			switch (config & X86_CFG_MUL_OP_BITS) {
				case X86_CFG_ADD:
					reg = 0;
					break;
				case X86_CFG_SUB:
					reg = 5;
					break;
				default: SI_PANIC();
			}
			config &= ~X86_CFG_MUL_OP_BITS;
		}

		switch (config & X86_CFG_SRC_BITS) {
			case X86_CFG_NOTATION_2:
				reg = 2;
				break;

			case X86_CFG_NOTATION_3:
				reg = 3;
				break;

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

		if (mod == X86_MOD_RM_MOD_M8) {
			u8 val = (config & X86_CFG_SRC_M) ? (u8)src : (u8)dst;
			if ((config & X86_CFG_SRC_M_NOT_NEG) == 0) {
				val = -val;
			}
			memcpy(&out[i], &val, 1), i += 1;
		}
		else if (mod == X86_MOD_RM_MOD_M32) {
			SI_PANIC();
			u32 val = (config & X86_CFG_SRC_M) ? src : dst;
			if ((config & X86_CFG_SRC_M_NOT_NEG) == 0) {
				val = -val;
			}
			memcpy(&out[i], &val, 4), i += 4;
		}

	}

	switch (config & X86_CFG_INTWORD_BITS) {
		case 0: break;

		case X86_CFG_IB:
			memcpy(&out[i], &src, sizeof(u8)), i += 1;
			break;
		case X86_CFG_IW:
			memcpy(&out[i], &src, sizeof(u16)), i += 2;
			break;
		case X86_CFG_ID:
			memcpy(&out[i], &src, sizeof(u32)), i += 4;
			break;
		default: SI_PANIC();
	}

	for_range (j, 0, i) {
		SI_LOG_FMT("%ll02X ", out[j]);
	}
	SI_LOG("\n");

	state->len += i;
}


x86Register sc_x86PickFunctionArg(x86EnvironmentState* state, scAsmRegister reg) {
	SI_ASSERT(si_betweenu(reg, SC_ASM_REG_PARAM_0, SC_ASM_REG_PARAM_MAX));

	switch (state->conv) {
		case X86_CALLING_CONV_SYSTEM_V_X86: {
			u32 regs[] = {RDI, RSI, RDX, RCX, R8, R9};

			u32 value = reg - SC_ASM_REG_PARAM_0;
			if (value < countof(regs)) {
				return regs[value];
			}
			si_printf("LEN: %i - %i\n", value, countof(regs));
			/* TODO(EimaMei): Suprogramuoti atvėjį, kai funkcijos parametrų ilgis
			 * viršija daugiau negu 6, t. y., kai likusieji parametrai yra rietuvėje.*/
			SI_PANIC();
		}
		default: SI_PANIC();
	}
}



x86Register sc_x86RegisterConvert(x86EnvironmentState* x86, scAsmRegister reg) {
	switch (x86->conv) {
		case X86_CALLING_CONV_SYSTEM_V_X86: {
			switch (reg) {
				case SC_ASM_REG_RET: return RAX;
				default: {
					SI_ASSERT_FMT(si_between(reg, SC_ASM_REG_0, SC_ASM_REG_PARAM_MAX), "Reg '%i/%i' doesn't exist", reg, reg - SC_ASM_REG_0);

					u32 value = reg - SC_ASM_REG_0;
					return (value < 16)
						? value
						: sc_x86PickFunctionArg(x86, reg);
				}
			}
			break;
		}
		default: SI_PANIC();
	}

	return reg;
}

x86Register sc_x86PickAvailableReg(x86EnvironmentState* x86) {
	scAsmRegister reg = SC_ASM_REG_0;
	while (!sc_asmRegisterAvailable(x86->root, reg)) { reg += 1; }

	x86Register value = reg - SC_ASM_REG_0;
	return value;
}


/* ========================================================================== */

#define X86_ASM_TEMPLATE(x86, type, opcode, extra, extra8bit, extra16bit, extra32bit, \
		extra64bit, dst, src, func) \
	case (type): \
		func(x86, opcode, dst, src, (extra) |(extra8bit)); \
		break; \
	case (type) + 1: \
		func(x86, opcode, dst, src, (extra) | (extra16bit)); \
		break; \
	case (type) + 2: \
		func(x86, (opcode) + 1, dst, src, (extra) | (extra32bit)); \
		break; \
	case (type) + 3: \
		func(x86, (opcode) + 1, dst, src, X86_CFG_64BIT | (extra) | (extra64bit)); \
		break;


#define X86_ASM_TEMPLATE__NEW(x86, type, opcode, instruction, extra, func) \
	case type + 0: sc_x86Opcode__ ## func(x86, opcode + 0, instruction, extra); break; \
	case type + 1: sc_x86Opcode__ ## func(x86, opcode + 1, instruction, extra); break; \
	case type + 2: sc_x86Opcode__ ## func(x86, opcode + 1, instruction, extra); break; \
	case type + 3: sc_x86Opcode__ ## func(x86, opcode + 1, instruction, extra | X86_CFG_64BIT); break;
#define X86_ASM_TEMPLATE_INT__NEW(x86, type, opcode, instruction, extra, func) \
	case type + 0: sc_x86Opcode__ ## func(x86, opcode + 0, instruction, extra | X86_CFG_IB); break; \
	case type + 1: sc_x86Opcode__ ## func(x86, opcode + 1, instruction, extra | X86_CFG_IW); break; \
	case type + 2: sc_x86Opcode__ ## func(x86, opcode + 1, instruction, extra | X86_CFG_ID); break; \
	case type + 3: sc_x86Opcode__ ## func(x86, opcode + 1, instruction, extra | X86_CFG_IQ | X86_CFG_64BIT); break;


#define X86_ASM_TEMPLATE_RMB(x86, type, opcode, extra, extra8bit,  extra16bit, \
		extra32bit, extra64bit, dst, src, func) \
	X86_ASM_TEMPLATE( \
		x86, type, opcode, (X86_CFG_RMB) | (extra), \
		extra8bit, extra16bit, extra32bit, extra64bit, dst, src, \
		func \
	)

#define X86_ASM_TEMPLATE_RMB__REG(x86, type, opcode, instruction, extra) \
	X86_ASM_TEMPLATE__NEW(x86, type, opcode, instruction, extra, REG_RMB)

#define X86_ASM_TEMPLATE__REG_REG_RMB(x86, type, opcode, instruction) \
	X86_ASM_TEMPLATE__NEW(x86, type, opcode, instruction, 0, REG_REG_RMB)
#define X86_ASM_TEMPLATE__REG_RMB_REG(x86, type, opcode, instruction) \
	X86_ASM_TEMPLATE__NEW(x86, type, opcode, instruction, 0, REG_RMB_REG)

#define X86_ASM_TEMPLATE__REG_MEM_RMB(x86, type, opcode, instruction) \
	X86_ASM_TEMPLATE__NEW(x86, type, opcode, instruction, 0, REG_MEM_RMB)
#define X86_ASM_TEMPLATE__REG_RMB_INT_EX(x86, type, opcode, instruction, extra) \
	X86_ASM_TEMPLATE_INT__NEW(x86, type, opcode, instruction, extra, REG_RMB_INT)

#define X86_ASM_TEMPLATE__REG_RMB_INT(x86, type, opcode, instruction) \
	X86_ASM_TEMPLATE__REG_RMB_INT_EX(x86, type, opcode, instruction, X86_CFG_ADD)




#define X86_ASM_TEMPLATE_RMB__MEM(x86, type, opcode, instruction, extra) \
	X86_ASM_TEMPLATE__NEW(x86, type, opcode, instruction, extra, MEM_RMB)

#define X86_ASM_TEMPLATE_RMB__MEM_REG(x86, type, opcode, instruction) \
	X86_ASM_TEMPLATE_RMB( \
		x86, type, opcode, X86_CFG_DST_M | X86_CFG_SRC_R, 0, 0, 0, 0, \
		(instruction)->dst, sc_x86RegisterConvert(x86, (instruction)->src), \
		sc_x86Opcode \
	)
#define X86_ASM_TEMPLATE_RMB__MEM_MEM_EX(x86, type, opcode, instruction, extra) \
	case (type): { \
		x86Register reg = sc_x86PickAvailableReg(x86); \
		sc_x86Opcode( \
			x86, X86_MOV_R8_RM8, reg, (instruction)->src, X86_CFG_RMB | X86_CFG_DST_R | X86_CFG_SRC_M \
		); \
		sc_x86Opcode( \
			x86, opcode, (instruction)->dst, reg, X86_CFG_RMB | X86_CFG_DST_M | X86_CFG_SRC_R | (extra) \
		); \
		break; \
	} \
	case (type) + 1:  \
	case (type) + 2: { \
		x86Register reg = sc_x86PickAvailableReg(x86); \
		sc_x86Opcode( \
			x86, X86_MOV_R8_RM8 + 1, reg, (instruction)->src, X86_CFG_RMB | X86_CFG_DST_R | X86_CFG_SRC_M \
		); \
		sc_x86Opcode( \
			x86, (opcode) + 1, (instruction)->dst, reg, X86_CFG_RMB | X86_CFG_DST_M | X86_CFG_SRC_R | (extra) \
		); \
		break; \
	} \
	case (type) + 3: { \
		x86Register reg = sc_x86PickAvailableReg(x86); \
		sc_x86Opcode( \
			x86, X86_MOV_R8_RM8 + 1, reg, (instruction)->src, X86_CFG_64BIT | X86_CFG_RMB | X86_CFG_DST_R | X86_CFG_SRC_M \
		); \
		sc_x86Opcode( \
			x86, (opcode) + 1, (instruction)->dst, reg, X86_CFG_64BIT | X86_CFG_RMB | X86_CFG_DST_M | X86_CFG_SRC_R | (extra) \
		); \
		break; \
	}
#define X86_ASM_TEMPLATE_RMB__MEM_MEM(x86, type, opcode, instruction) \
	X86_ASM_TEMPLATE_RMB__MEM_MEM_EX(x86, type, opcode, instruction, 0)

#define X86_ASM_TEMPLATE_RMB__MEM_ID_EX(x86, type, opcode, instruction, extra) \
	X86_ASM_TEMPLATE_RMB( \
		x86, type, opcode, X86_CFG_DST_M | (extra), X86_CFG_IB, X86_CFG_IW, X86_CFG_ID, 0, \
		(instruction)->dst, (instruction)->src, \
		sc_x86Opcode \
	)
#define X86_ASM_TEMPLATE_RMB__MEM_ID(x86, type, opcode, instruction) \
	X86_ASM_TEMPLATE_RMB__MEM_ID_EX(x86, type, opcode, instruction, X86_CFG_ADD)
