#!/usr/bin/perl -w

use strict;

#my $limit = 5000000;
my $limit = 500000;
my @vec = 1..($limit-1);

$vec[0] = 0;
my $prime_count = 0;

for (my $i = 0; $i < $limit; $i++) {
    if ($vec[$i] != 0) {
	$prime_count++;
	my $prime = $i + 1;
	for (my $j = 0; $j < $limit; $j += $prime) {
	    $vec[$j] = 0;
	}
    }
}

print "There are ", $prime_count, " primes less than or equal to ", $limit,
    ".\n";
