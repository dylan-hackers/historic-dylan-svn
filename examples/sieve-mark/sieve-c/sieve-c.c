#include <stdio.h>

int main(int argc, char **argv) {
    int limit;
    int *vec;
    int prime_count;
    int i, j, prime;

    if (argc != 2) {
	fprintf(stderr, "Usage: sieve-c limit\n");
	exit(1);
    }

    limit = atoi(argv[1]);
    vec = (int*) malloc(limit * sizeof(int));
    
    for (i = 0; i < limit; i++) {
	vec[i] = i+1;
    }

    vec[0] = 0;
    prime_count = 0;

    for (i = 1; i < limit; i++) {
	if (vec[i] != 0) {
	    prime_count++;
	    prime = i + 1;
	    for (j = i + prime; j < limit; j += prime) {
		vec[j] = 0;
	    }
	}
    }

    printf("There are %d primes less than or equal to %d.\n",
	   prime_count, limit);
}
