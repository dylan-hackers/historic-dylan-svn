#!/usr/local/bin/perl -w

@testfiles =
    qw(tests/test1
       tests/test2
       tests/test3
       tests/test4
       tests/test5
       tests/test6
       tests/test8
       tests/example.txt
       tests/test7
       );

for my $f (@testfiles){
    print "testing $f: ";
    system("rm -f $f.out");
    system("./icfp2001 180 <$f >$f.out 2>$f.debug");
    system("./validate/validate $f $f.out");
}
