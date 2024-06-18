# Sili C Compiler
A pet project of mine that tries to (loosely) implement the C89 standard for x86-64 Linux.

# Goal
Main goal is to obviously make the compiler C89 standard compliant. If that were to happen, C99 would be the next target. The most ultimate goal is to make the compiler self-hosted without GCC or clang (i dont recognize msvc as a valid C compiler).

# TODOs
This section isn't meant to taken 100% seriously, there are a lot of other components that I'm probably missing:

## C89 implementation
- [ ] variable declarations:
    - [ ] `int x;`
    - [x] `int x = <int>;`
    - [x] `int x = <identifier (static/non-static)>;`
    - [x] `int x = <identifier/int> <+|-> <identifier/int>;`
    - [x] `int x = <identifier/int> <+|-> <identifier/int> <+|-> <identifier/int>...`
    - [x] `,` operator use (`NOTE:` Doesn't assert when a variable doesn't exist if it's declared later in the comma chain).
- [ ] scopes
- [ ] pointers
- [ ] `(` and `)` usage
- [ ] Casting
- [ ] functions:
    - [ ] K&R function declarations/implementations.
    - [ ] `void` as a declaration
    - [x] Function implementations
    - [x] Function declarations
    - [ ] Function calls
- [ ] unary operators:
    - [x] `+` nothing
    - [x] `-` NEG
    - [x] `~` bitwise complement
    - [ ] `!` NOT (NOTE: Works for constants only)
    - [ ] `&` Address
    - [ ] `*` Dereference
- [ ] arithmetic operators:
    - [x] `+` add
    - [x] `-` sub
    - [ ] `*` mul
    - [ ] `/` div
    - [ ] `%` mod
- [ ] bit operators:
    - [ ] `|` OR
    - [ ] `~` XOR
    - [ ] `&` AND
    - [ ] `<<` left shift
    - [ ] `>>` right shift
- [ ] comparison operators:
    - [ ] `==` equal
    - [ ] `!=` not equal
    - [ ] `<` less
    - [ ] `<=` less or equal
    - [ ] `>` greater
    - [ ] `>=` greater or equal
    - [ ] `&&` short-circuiting logical and
    - [ ] `||` hort-circuiting logical or
- [ ] logical operators:
    - [ ] `&&` conditional AND
    - [ ] `||` conditional OR
    - [ ] `!` NOT
- [ ] `=` assign operator
- [ ] compound operators:
    - [x] `++` add one:
        - [x] `++x` front
        - [x] `x++` back
    - [x] `--` sub one:
        - [x] `--x` front
        - [x] `x--` back
    - [x] `+=` add and assign
    - [ ] `-=` sub and assign (NOTE: Works for non-binary arguments, binary arguments however are broken)
    - [ ] `*=` mul and assign
    - [ ] `/=` div and assign
    - [ ] `|=` OR and assign
    - [ ] `~=` XOR and assign
    - [ ] `&=` AND and assign
    - [ ] `<<=` left shift and assign
    - [ ] `>>=` right shift and assign
- [ ] integer constants:
    - [x] decimal-constant (`non-zero digit` and `decimal-constant digit(s)`, if any)
    - [ ] octal-constant (`0` and `octal-constant digit(s)`, if any)
    - [ ] hexadecimal-constant (`0x` and `hexadecimal-constant digit(s)`, if any)
    - [x] suffixes:
        - [x] unsigned (`u`, `U`)
        - [x] long (`l`, `L`)
    - [ ] BONUS, UNSTANDARD C FEATURES:
        - [ ] binary-constant (`0b` and `binary-constant digit(s)`, if any)
        - [ ] proper octal-constant (`0o` and `octal-constant digit(s)`, if any)
- [x] character constants:
    - [x] c-char (`'A'`...`'Z'`)
    - [x] escape sequences:
        - [x] simple (`\'`, `\"`, `\?`, `\\`, `\a`, `\b`, `\f`, `\n`, `\r`, `\t`, `\v`)
        - [x] octal (`\NN`)
        - [x] hexadecimal (`\xNN`)
    - [x] long versions (`L'A'`, `L'\\'`, ...)
- [ ] trigraph sequences (`??=`, `??(`, `??/`, `??)`, `??'`, `??<`, `??!`, `??>`, `??-`)
- [ ] keywords:
    - [ ] auto
    - [ ] break
    - [ ] case
    - [x] char
    - [ ] const
    - [ ] continue
    - [ ] default
    - [ ] do
    - [ ] double
    - [x] int
    - [ ] else
    - [x] long
    - [ ] enum
    - [ ] register
    - [ ] extern
    - [ ] return:
        - [ ] System V AMD64:
            - [x] 8-bit, 16-bit returns
            - [x] 32-bit returns
            - [ ] 64-bit returns
            - [ ] 128-bit, 256-bit, etc returns
    - [ ] float
    - [x] short
    - [ ] for
    - [x] signed
    - [ ] goto
    - [ ] sizeof
    - [ ] if
    - [ ] static
    - [ ] struct
    - [ ] switch
    - [x] typedef
    - [ ] union
    - [x] unsigned
    - [ ] void
    - [ ] volatile
- [ ] Variadic functions

## Platform support
- [ ] x86
    - [ ] AMD64 (x86-64 for Unix, x64 for Windows)
        - [x] Linux:
            - [x] `_start` entrypoint
            - [x] executable file generation
            - [x] `argc`, `argv`
        - [ ] MacOS
        - [ ] Windows
        - [ ] Other Unix OSses
    - [ ] i386 (x86-32 for Unix, x86 for Windows)
        - [ ] Linux
        - [ ] MacOS
        - [ ] Windows
        - [ ] Other Unix OSses

## Environmental limits
### Translation limits
**NOTE 1:** As of 2024-06-13, the compiler  isn't being tested for  translation
limits yet. This section is kept as a mere a reminder.
**NOTE 2:** These limits are directly taken from section 5.2.4.1 of the C89
standard draft.

- [ ] 15 nesting levels of compound statements, iteration control structures, and
selection control structures
- [ ] 8 nesting levels of conditional inclusion
- [ ] 12 pointer, array, and function declarators (in any combinations) modifying
an arithmetic, a structure, a union or an incomplete type in a declaration
- [ ] 31 nesting levels of parenthesized declarators within a full declarator
- [ ] 32 nesting levels of parenthesized expressions within a full expression
- [ ] 31 significant initial characters in an internal identifier or a macro name
- [ ] 6 significant initial characters in an external identifier
- [ ] 511  external identifiers in one translation unit
- [ ] 127 identifiers with block scope declared in one block
- [ ] 1024 macro identifiers simultaneously defined in one translation unit
- [ ] 31 parameters in one function definition
- [ ] 31 arguments in one function call
- [ ] 31 parameters in one macro definition
- [ ] 31 arguments in one macro invocation
- [ ] 509 characters in a logicai source line
- [ ] 509 characters in a character string literal or wide string literal (after concatenation)
- [ ] 32767 bytes in an object (in a hosted environment only)
- [ ] 8 nesting levels for #included files
- [ ] 257 case labels for a switch statement (excluding those for any nested switch
statements)
- [ ] 127 members in a single structure or union
- [ ] 127 enumeration constants in a single enumeration
- [ ] 15 levels of nested structure or union definitions in a single struct-declaration-list

## Command-line arguments
- [ ] Make it possible to change the input/output files.

## Other
- [ ] Fix `si_realloc` for the compiler

# Credits (for fun)
[ANSI C89/ISO C90 standard draft](https://web.archive.org/web/20200909074736if_/https://www.pdf-archive.com/2014/10/02/ansi-iso-9899-1990-1/ansi-iso-9899-1990-1.pdf) - C89 standard PDF.

[Intel x86 Opcode Table and Reference](https://shell-storm.org/x86doc/) - a good source for checking the list of valid opcodes on x86-64 (don't even try to use the official Intel manual).

[Online Assembler and Disassembler](https://shell-storm.org/online/Online-Assembler-and-Disassembler/) - an online disassembler to easily check if the assembly output is correct.
