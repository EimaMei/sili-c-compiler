# Sili C Compiler
A pet project of mine that tries to (loosely) implement the C89 standard for x86-64 Linux.

# Goal
Main goal is to obviously make the compiler C89 standard compliant. If that were to happen, C99 would be the next target. The most ultimate goal is to make the compiler self-hosted without GCC or clang (i dont recognize msvc as a valid C compiler).

# Features/TODO
This section isn't meant to taken 100% seriously, there are a lot of other components that I'm probably missing:
- [x] `argc`, `argv`
- [ ] variable declarations:
    - [ ] `int x;`
    - [x] `int x = <int>;`
    - [x] `int x = <identifier (static/non-static)>;`
    - [x] `int x = <identifier/int> <+|-> <identifier/int>;`
    - [x] `int x = <identifier/int> <+|-> <identifier/int> <+|-> <identifier/int>...`
    - [x] `,` operator use (`NOTE:` Doesn't assert when a variable doesn't exist if it's declared later in the comma chain).
- [ ] `void` in general
- [ ] scopes
- [ ] pointers
- [ ] structures
- [ ] unions
- [ ] `(` and `)` usage
- [ ] Casting
- [ ] return:
    - [x] 8-bit, 16-bit returns
    - [x] 32-bit returns
    - [ ] 64-bit returns
    - [ ] 128-bit, 256-bit, etc returns
- [ ] functions:
    - [ ] K&R function declarations/implementations.
    - [ ] `void` as a declaration
    - [x] Function implementations
    - [x] Function declarations
    - [ ] Function calls
- [ ] unary operators:
    - [ ] `+` technically nothing?
    - [ ] `-` negation (NOTE: Doesn't work if the expression is not binary)
    - [ ] `~` bitwise complement
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
- [ ] `if` statements
- [ ] `do` statements
- [ ] `switch` statements
- [x] `typedef`
- [ ] Variadic functions
- [x] `_start` entrypoint for x86-64 Linux.

## Other TODOs
- Fix `si_realloc` for this compiler

# Credits (for fun)
[ANSI C89/ISO C90 standard draft](https://web.archive.org/web/20200909074736if_/https://www.pdf-archive.com/2014/10/02/ansi-iso-9899-1990-1/ansi-iso-9899-1990-1.pdf) - C89 standard PDF.
[Intel x86 Opcode Table and Reference](https://shell-storm.org/x86doc/) - a good source for checking the list of valid opcodes on x86-64 (don't even try to use the official Intel manual).
[Online Assembler and Disassembler](https://shell-storm.org/online/Online-Assembler-and-Disassembler/) - an online disassembler to easily check if the assembly output is correct.
