#!/usr/bin/perl -w

use strict;

my $limit = 5000000;
#my $limit = 500000;
my @vec = 0..($limit-1);

$vec[0] = 0;
$vec[1] = 0;
my $prime_count = 0;

for (my $i = 2; $i < $limit; $i++) {
    if ($vec[$i] != 0) {
	$prime_count++;
	for (my $j = 2*$i; $j < $limit; $j += $i) {
	    $vec[$j] = 0;
	}
    }
}

print "There are ", $prime_count, " primes less than or equal to ", $limit,
    ".\n";
