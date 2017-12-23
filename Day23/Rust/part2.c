#include <stdio.h>
#include <stdlib.h>

/*
 * Steps taken:
 *   1) Translated from assembly (part2.direct.c)
 *   2) Identify branchings and loops
 *   3) Turn into idiomatic-ish C
 *   4) Optimize inner loop
 *
 * Result: Count all non-prime (composite) numers in steps of 17 between 106500 and 123500 inclusive
 */
int main(int argc, char *argv[])
{
	long composites = 0;

	// Loop from lower bound to upper bound in steps of 17
	// NB: 123500 - 106500 == 17000 => 1000 iterations
	for (int b = 106500; b <= 123500; b += 17) {
		// Loop over the lower half of the numeric range of the loop variable
		for (int d = 2; d < b/2; ++d) {
			// Is number evenly dividable by some number?
			if (b % d == 0) {
				// Ye, not a prime, count it
				++composites;
				break;
			}
		}
	}

	printf("%d\n", composites);

	return 0;
}
