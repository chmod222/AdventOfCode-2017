#include <stdio.h>
#include <stdlib.h>

/*
 * Really want goto for this one, sorry Rust.
 */
int main(int argc, char *argv[])
{
	long a = 1, b = 0, c = 0, d = 0, e = 0, f = 0, g = 0, h = 0;

	b = 65; // set b 65
	c = b;  // set c b

	// jnz a 2
	if (a) {
		goto jnza2;
	}

	// jnz 1 5
	if (1) {
		goto jnz15;
	}

jnza2:
	b *= 100;     // mul b 100
	b -= -100000; // sub b -100000

	c = b;        // set c b
	c -= -17000;  // sub c -17000

jnz15:
jnz1m23:
	f = 1; // set f 1
	d = 2; // set d 2

jnzgm13:
	e = 2; // set e 2;

jnzgm8:
	g = d;  // set g d
	g *= e; // mul g e
	g -= b; // sub g b

	// jnz g 2
	if (g) {
		goto jnzg2;
	}

	f = 0; // set f 0

jnzg2:
	e -= -1; // sub e -1
	g = e;   // set g e
	g -= b;  // sub g b

	// jnz g -8
	if (g) {
		goto jnzgm8;
	}

	d -= -1; // sub d -1
	g = d;   // set g d
	g -= b;  // sub g b

	// jnz g -13
	if (g) {
		goto jnzgm13;
	}

	// jnz f 2
	if (f) {
		goto jnzf2;
	}

	h -= -1; // sub h -1

jnzf2:
	g = b;  // set g b
	g -= c; // sub g c

	// jnz g 2
	if (g) {
		goto jnzg2_2;
	}

	// jnz 1 3
	if (1) {
		goto out;
	}

jnzg2_2:
	b -= -17; // sub b -17
	
	// jnz 1 -23
	if (1) {
		goto jnz1m23;
	}

out:
	return h;
}
