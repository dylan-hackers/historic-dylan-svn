#include <iostream>
#include <vector>
#include <algorithm>

std::vector<int> vec;

int prime_count = 0;

void sieve(std::vector<int>::iterator elem)
{
    if (*elem == 0) { return; }

    int prime = *elem;

    // We don't count 0 or 1
    if ( (prime == 0) || (prime == 1))
        return;

    prime_count++;

    for(std::vector<int>::iterator pos = elem + prime; 
		    pos <= vec.end(); pos += prime)
    {
        *pos = 0;
    }
};

int main(int argc, char **argv) 
{
    if (argc != 2) {
	cerr << "Usage: sieve-c limit\n" << endl;
	exit(1);
    }

    int limit = atoi(argv[1]);

    vec.reserve(limit);
    for(int i = 0; i < limit; ++i)
    {
	    vec.push_back(i + 1);
    }

    int prime;
    
    std::vector<int>::iterator pos =vec.begin();
    for( ; pos != vec.end(); ++pos)
    {
	    sieve(pos);
    }

    cerr << "There are " << prime_count << " primes less than or equal to "
	   << limit << "." << endl;
}
