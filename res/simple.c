int func(int x);

int main(int argc, char** argv) {
	return 2 + 2 + argc;
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

