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

sub next_color {
    my $col = shift() + 1;
    $col = 1 if $col > 3;
    $col;
}

sub color_max {
    my ($a, $b) = @_;
    if ($a == 1){
	if ($b == 2){$b} else {$a};
    } elsif ($a == 2){
	if ($b == 3){$b} else {$a};
    } else {
	if ($b == 1){$b} else {$a};
    }
}

sub color_min {
    my ($a, $b) = @_;
    if ($a == 1){
	if ($b == 3){$b} else {$a};
    } elsif ($a == 2){
	if ($b == 1){$b} else {$a};
    } else {
	if ($b == 2){$b} else {$a};
    }
}

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
		    if ($left == next_color($ahead)){
			$newstate = $left;
		    } else {
			$newstate = next_color($left);
		    }
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
		    #$newstate = next_color(color_min($left, $right));
		} elsif ($count[1] == 2){
		    $newstate = 2;
		} elsif ($count[2] == 2){
		    $newstate = 3;
		} elsif ($count[3] == 2){
		    $newstate = 1;
		} elsif ($left == 0){
		    my $big = color_max($ahead, $right);
		    $newstate = $big;
		    #$nonhomeaction = "forage_onblank_check_rock_left$big:";
		} elsif ($right == 0){
		    my $big = color_max($ahead, $left);
		    $newstate = $big;
		    #$nonhomeaction = "forage_onblank_check_rock_right$big:";
		} else {
		    die("I didn't think I could get here");
		}
	    } elsif ($count[0] = -1){
		if ($left != 0){
		    #$newstate = next_color($left);
		    $nonhomeaction = "move_turn_left_and_forage:";
		}
		#if ($right != 0){
		#    $nonhomeaction = "move_turn_right_and_forage:";
		#}
	    }
	    $nonhomeaction = "forage_onblank_setstate$newstate:" if $newstate;
		    

	    print $prefix."_",$twobit[$left],$twobit[$ahead],$twobit[$right],":\t";
	    printf("Sense Here %s %s Home\n", $homeaction, $nonhomeaction);
	}
    }
}
