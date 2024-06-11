typedef int i32;
typedef char* cstring;

i32 func(i32 x);


i32 main(i32 argc, cstring* argv) {
	/* int x = -222 + 2 + argc + -2 + argc + argc + argc + argc + 2 + 222; */
	i32 y = 50 + 50 + argc;
	int x = argc;
	return y + x;
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

