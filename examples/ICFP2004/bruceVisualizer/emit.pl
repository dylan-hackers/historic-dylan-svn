#!/usr/bin/env perl

use strict;

my $prefix = "forage_onblank";

sub emitjumptable {
    my $known = shift;
    my @tests = @_;
    my $bit = length($known);
    
    if ($bit < @tests){
	my $statement = $tests[$bit];
	my $label = $prefix . "_$known";
	"$label:\t" . sprintf($statement, $label."1:", $label."0:") . "\n" .
	    emitjumptable($known."0", @tests) .
	    emitjumptable($known."1", @tests);
    }
}

my $code = emitjumptable
    ("",
     "Sense LeftAhead %s %s Marker 0",
     "Sense LeftAhead %s %s Marker 1",
     "Sense Ahead %s %s Marker 0",
     "Sense Ahead %s %s Marker 1",
     "Sense RightAhead %s %s Marker 0",
     "Sense RightAhead %s %s Marker 1");
print $code;

my @twobit = ("00", "01", "10", "11");

for my $left (0..3){
    for my $ahead (0..3){
	for my $right (0..3){

	    # if all three are non-zero then we become 1 more than the min
	    my $homeaction = "turn_and_forage:";
	    my @sorted = sort($left, $ahead, $right);
	    my $min = $sorted[0];
	    my @count;
	    $count[$left]++;
	    $count[$ahead]++;
	    $count[$right]++;

	    if ($count[0] == 0){
		my $newstate;
		if ($count[1] == 3){
		    $newstate = 3;
		} elsif ($count[2] == 3){
		    $newstate = 1;
		} elsif ($count[3] == 3){
		    $newstate = 2;
		} elsif ($count[1] == 1){
		    $newstate = 1;
		} elsif ($count[2] == 1){
		    $newstate = 2;
		} elsif ($count[3] == 1){
		    $newstate = 3;
		} else {
		    die("shouldn't happen!");
		}
		$homeaction = "forage_onblank_setstate$newstate:";
	    }

	    my $nonhomeaction = "turn_and_forage:";
	    my $newstate;
	    if ($count[0] == 0){
		if ($left == $right){
		    $newstate = $left; #maybe $ahead + 1?
		} elsif ($count[1] == 2){
		    $newstate = 2;
		} elsif ($count[2] == 2){
		    $newstate = 3;
		} elsif ($count[3] == 2){
		    $newstate = 1;
		}
	    } elsif ($count[0] == 1){
		if ($ahead == 0){
		    $nonhomeaction = "move_and_forage:";
		} elsif ($count[1] == 2){
		    $newstate = 2;
		} elsif ($count[2] == 2){
		    $newstate = 3;
		} elsif ($count[3] == 2){
		    $newstate = 1;
		} elsif ($ahead == 0){
		    $nonhomeaction = "move_and_forage:";
		}
	    } elsif ($count[0] = 2){
		if ($left != 0 || $right != 0){
		    $nonhomeaction = "move_and_forage:";
		}
	    }
	    $nonhomeaction = "forage_onblank_setstate$newstate:" if $newstate;
		    

	    print $prefix."_",$twobit[$left],$twobit[$ahead],$twobit[$right],":\t";
	    printf("Sense Here %s %s Home\n", $homeaction, $nonhomeaction);
	}
    }
}
