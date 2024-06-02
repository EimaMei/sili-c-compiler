#include <sili.h>

/* Indexes into the e_ident array.  Keep synced with
   http://www.sco.com/developers/gabi/latest/ch4.eheader.html */
#define	EI_MAG0		0	/* Magic number, byte 0. */
#define	EI_MAG1		1	/* Magic number, byte 1. */
#define	EI_MAG2		2	/* Magic number, byte 2. */
#define	EI_MAG3		3	/* Magic number, byte 3. */
#define	EI_CLASS	4	/* Class of machine. */
#define	EI_DATA		5	/* Data format. */
#define	EI_VERSION	6	/* ELF format version. */
#define	EI_OSABI	7	/* Operating system / ABI identification */
#define	EI_ABIVERSION	8	/* ABI version */
#define	OLD_EI_BRAND	8	/* Start of architecture identification. */
#define	EI_PAD		9	/* Start of padding (per SVR4 ABI). */
#define	EI_NIDENT	16	/* Size of e_ident array. */


/* Values for e_ident[EI_VERSION] and e_version. */
#define	EV_NONE		0
#define	EV_CURRENT	1

/* Values for e_ident[EI_CLASS]. */
#define	ELFCLASSNONE	0	/* Unknown class. */
#define	ELFCLASS32	1	/* 32-bit architecture. */
#define	ELFCLASS64	2	/* 64-bit architecture. */

/* Values for e_ident[EI_DATA]. */
#define	ELFDATANONE	0	/* Unknown data format. */
#define	ELFDATA2LSB	1	/* 2's complement little-endian. */
#define	ELFDATA2MSB	2	/* 2's complement big-endian. */

/* Values for e_ident[EI_OSABI]. */
#define	ELFOSABI_NONE		0	/* UNIX System V ABI */
#define	ELFOSABI_HPUX		1	/* HP-UX operating system */
#define	ELFOSABI_NETBSD		2	/* NetBSD */
#define	ELFOSABI_LINUX		3	/* GNU/Linux */
#define	ELFOSABI_HURD		4	/* GNU/Hurd */
#define	ELFOSABI_86OPEN		5	/* 86Open common IA32 ABI */
#define	ELFOSABI_SOLARIS	6	/* Solaris */
#define	ELFOSABI_AIX		7	/* AIX */
#define	ELFOSABI_IRIX		8	/* IRIX */
#define	ELFOSABI_FREEBSD	9	/* FreeBSD */
#define	ELFOSABI_TRU64		10	/* TRU64 UNIX */
#define	ELFOSABI_MODESTO	11	/* Novell Modesto */
#define	ELFOSABI_OPENBSD	12	/* OpenBSD */
#define	ELFOSABI_OPENVMS	13	/* Open VMS */
#define	ELFOSABI_NSK		14	/* HP Non-Stop Kernel */
#define	ELFOSABI_AROS		15	/* Amiga Research OS */
#define	ELFOSABI_ARM		97	/* ARM */
#define	ELFOSABI_STANDALONE	255	/* Standalone (embedded) application */

/* Values for e_type. */
#define	ET_NONE		0	/* Unknown type. */
#define	ET_REL		1	/* Relocatable. */
#define	ET_EXEC		2	/* Executable. */
#define	ET_DYN		3	/* Shared object. */
#define	ET_CORE		4	/* Core file. */
#define	ET_LOOS		0xfe00	/* First operating system specific. */
#define	ET_HIOS		0xfeff	/* Last operating system-specific. */
#define	ET_LOPROC	0xff00	/* First processor-specific. */
#define	ET_HIPROC	0xffff	/* Last processor-specific. */

/* Values for e_machine. */
#define	EM_NONE		0	/* Unknown machine. */
#define	EM_M32		1	/* AT&T WE32100. */
#define	EM_386		3	/* Intel i386. */
#define	EM_PPC		20	/* PowerPC 32-bit. */
#define	EM_PPC64	21	/* PowerPC 64-bit. */
#define	EM_ARM		40	/* ARM. */
#define	EM_X86_64	62	/* Advanced Micro Devices x86-64 */
#define	EM_AARCH64	183	/* AArch64 (64-bit ARM) */

/* Flags for sh_flags. */
#define	SHF_WRITE		0x1	/* Section contains writable data. */
#define	SHF_ALLOC		0x2	/* Section occupies memory. */
#define	SHF_EXECINSTR		0x4	/* Section contains instructions. */
#define	SHF_MERGE		0x10	/* Section may be merged. */
#define	SHF_STRINGS		0x20	/* Section contains strings. */
#define	SHF_INFO_LINK		0x40	/* sh_info holds section index. */
#define	SHF_LINK_ORDER		0x80	/* Special ordering requirements. */
#define	SHF_OS_NONCONFORMING	0x100	/* OS-specific processing required. */
#define	SHF_GROUP		0x200	/* Member of section group. */
#define	SHF_TLS			0x400	/* Section contains TLS data. */
#define	SHF_MASKOS	0x0ff00000	/* OS-specific semantics. */
#define	SHF_MASKPROC	0xf0000000	/* Processor-specific semantics. */

/* Values for p_type. */
#define	PT_NULL		0	/* Unused entry. */
#define	PT_LOAD		1	/* Loadable segment. */
#define	PT_DYNAMIC	2	/* Dynamic linking information segment. */
#define	PT_INTERP	3	/* Pathname of interpreter. */
#define	PT_NOTE		4	/* Auxiliary information. */
#define	PT_SHLIB	5	/* Reserved (not used). */
#define	PT_PHDR		6	/* Location of program header itself. */
#define	PT_TLS		7	/* Thread local storage segment */
#define	PT_LOOS		0x60000000	/* First OS-specific. */
#define	PT_SUNW_UNWIND	0x6464e550	/* amd64 UNWIND program header */
#define	PT_GNU_EH_FRAME	0x6474e550
#define	PT_GNU_STACK	0x6474e551
#define	PT_GNU_RELRO	0x6474e552
#define	PT_DUMP_DELTA	0x6fb5d000	/* va->pa map for kernel dumps
					   (currently arm). */
#define	PT_LOSUNW	0x6ffffffa
#define	PT_SUNWBSS	0x6ffffffa	/* Sun Specific segment */
#define	PT_SUNWSTACK	0x6ffffffb	/* describes the stack segment */
#define	PT_SUNWDTRACE	0x6ffffffc	/* private */
#define	PT_SUNWCAP	0x6ffffffd	/* hard/soft capabilities segment */
#define	PT_HISUNW	0x6fffffff
#define	PT_HIOS		0x6fffffff	/* Last OS-specific. */
#define	PT_LOPROC	0x70000000	/* First processor-specific type. */
#define	PT_HIPROC	0x7fffffff	/* Last processor-specific type. */

/* Values for p_flags. */
#define	PF_X		0x1		/* Executable. */
#define	PF_W		0x2		/* Writable. */
#define	PF_R		0x4		/* Readable. */
#define	PF_MASKOS	0x0ff00000	/* Operating system-specific. */
#define	PF_MASKPROC	0xf0000000	/* Processor-specific. */

/* Values for p_flags. */
#define	PF_X		0x1		/* Executable. */
#define	PF_W		0x2		/* Writable. */
#define	PF_R		0x4		/* Readable. */
#define	PF_MASKOS	0x0ff00000	/* Operating system-specific. */
#define	PF_MASKPROC	0xf0000000	/* Processor-specific. */

typedef uint64_t	Elf64_Addr;
typedef uint16_t	Elf64_Half;
typedef uint64_t	Elf64_Off;
typedef int32_t		Elf64_Sword;
typedef int64_t		Elf64_Sxword;
typedef uint32_t	Elf64_Word;
typedef uint64_t	Elf64_Lword;
typedef uint64_t	Elf64_Xword;

typedef struct {
	unsigned char	e_ident[EI_NIDENT];	/* File identification. */
	Elf64_Half	e_type;		/* File type. */
	Elf64_Half	e_machine;	/* Machine architecture. */
	Elf64_Word	e_version;	/* ELF format version. */
	Elf64_Addr	e_entry;	/* Entry point. */
	Elf64_Off	e_phoff;	/* Program header file offset. */
	Elf64_Off	e_shoff;	/* Section header file offset. */
	Elf64_Word	e_flags;	/* Architecture-specific flags. */
	Elf64_Half	e_ehsize;	/* Size of ELF header in bytes. */
	Elf64_Half	e_phentsize;	/* Size of program header entry. */
	Elf64_Half	e_phnum;	/* Number of program header entries. */
	Elf64_Half	e_shentsize;	/* Size of section header entry. */
	Elf64_Half	e_shnum;	/* Number of section header entries. */
	Elf64_Half	e_shstrndx;	/* Section name strings section. */
} elf64ElfHeader;

typedef struct {
	u32	p_type;		/* Entry type. */
	u32	p_flags;	/* Access permission flags. */
	u64	p_offset;	/* File offset of contents. */
	u64	p_vaddr;	/* Virtual address in memory image. */
	u64	p_paddr;	/* Physical address (not used). */
	u64	p_filesz;	/* Size of contents in file. */
	u64	p_memsz;	/* Size of contents in memory. */
	u64	p_align;	/* Alignment in memory and file. */
} elf64ProgramHeader;

void elf64_elfHeaderMake(elf64ElfHeader* header, b32 le, u8 elfOsABI, u16 elfArch) {
	char identifier[EI_NIDENT] = {
		0x7F, 'E', 'L', 'F',
		ELFCLASS64,
		!le + 1,
		EV_CURRENT,
		elfOsABI,
		0, 0, 0, 0, 0, 0, 0, 0
	};

	memcpy(&header->e_ident, identifier, countof(identifier));

	header->e_type = ET_EXEC;
	header->e_machine = elfArch;
	header->e_version = 0x1;
	header->e_entry = 0x08048000 + sizeof(elf64ElfHeader) + sizeof(elf64ProgramHeader);
	header->e_phoff = sizeof(elf64ElfHeader);
	header->e_shoff = 0;
	header->e_flags = 0;
	header->e_ehsize = sizeof(elf64ElfHeader);
	header->e_phentsize = sizeof(elf64ProgramHeader);
	header->e_phnum = 1;
	header->e_shentsize = 0;
	header->e_shnum = 0;
	header->e_shstrndx = 0;
}


void elf64_programHeaderMake(elf64ProgramHeader* header, u64 fileSize) {
	header->p_type = PT_LOAD;
	header->p_offset = 0;
	header->p_flags = 5;
	header->p_vaddr = 0x08048000;
	header->p_paddr = 0x08048000;
	header->p_filesz = header->p_memsz = fileSize;
	header->p_align = 0x1000;
}
