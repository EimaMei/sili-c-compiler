
typedef signed char i8;
typedef signed short i16;
typedef signed int i32;
typedef signed long i64;

typedef unsigned char u8;
typedef unsigned short u16;
typedef unsigned int u32;
typedef unsigned long u64;


typedef char* cstring;

i32 func(i32 x);


i32 main(i32 argc, cstring* argv) {
	return -222 + 2 + ~argc;
}



/*

int func(int x) {
	return x;
}

int yy(int x);

int main(int argc, char** argv) {
    int x = 22,
		y = x + func(x) + yy(1),
		z = 1;

	return argc;
}

int yy(int x) {
	return x + 1;
} */

