#!/usr/bin/perl -w

use strict;

my $fdcompile = 'minimal-console-compiler';

my $source = shift;
if (!defined $source) {
    print STDERR "usage: fdsingle file.dylan\n";
    exit 1;
}

# Defaults
my @use_libraries = ('common-dylan', 'io');
my @use_modules   = ('common-dylan', 'format-out');

my $lidfile_line = 1;
open(SOURCE, '<', $source) || die "Can't open $source: $!";

my %contents = &parse_dylan_header(\*SOURCE, $source);

my $module = $contents{'module'};
my $library = $contents{'library'} || $module;

if(exists $contents{'use_libraries'}) {
    @use_libraries = split /\s*,\s*/, $contents{'use_libraries'};
    @use_modules   = split /\s*,\s*/, $contents{'use_modules'};
}

my $use_library_clauses = '';
foreach my $usel (@use_libraries) {
    $use_library_clauses .= "  use $usel;\n";
}

my $use_module_clauses = '';
foreach my $usem (@use_modules) {
    $use_module_clauses .= "  use $usem;\n";
}

open LID, '>', "$library.lid" || die "Can't create $library.lid: $!";
print LID <<"__END__";
Library: $library
Files: $library-library.dylan
       $source

__END__

close LID;

open LIB, '>', "$library-library.dylan"
    || die "Can't create $library-library.dylan: $!";
print LIB <<"__END__";
Module: Dylan-user

define library $library
$use_library_clauses;
end library;

define module $module
$use_module_clauses
end module;
__END__

exec $fdcompile, '-link-exe', '-build', "$library.lid";

# parse_dylan_header($fh, $file)
#
# Reads the Dylan header (keyword: value, ...) from the filehandle $fh
# which is already open.  (The file doesn't have to be a lid file, it
# could be something else like a platforms.descr) Returns an
# associative array where the keys are header keywords (mashed to
# lower case), and the values are the header values.
#
# In contrast to the Dylan version, keywords can not appear more than
# once.  If they do, the last value will be used.  Multi-line values
# are supported, though.
#
sub parse_dylan_header {
    my ($fh, $file) = @_;

    my %contents;
    my $last_keyword;		# for multi-line values

    while (<$fh>) {
        $lidfile_line = $lidfile_line + 1;
        # remember, in Perl "." is any character other than newline.
	s/\r//g;		# Get rid of bogus carriage returns
	
        if (/^\s*$/) {  # if blank line, break out of loop
            return %contents;
        } elsif (m|^//.*$|) {
            # comment line, ignore
        } elsif (/^\s+(.*)$/) {
            # Continuation line -- part of a multi-line value
            $contents{$last_keyword} .= ' ' . $1;
        } else {
            if(!/^([-A-Za-z0-9_!&*<>|^\$\%\@\?]+):\s*(.*)\s*$/) {
		print STDERR "$file:$lidfile_line: Warning: ",
		             "bad keyword line\n";
		next;
	    }
            my $keyword = $1;
            my $value = $2;
            if ($value eq '#f' | $value eq '#F') {
                $value = 0;
            } elsif ($value eq '#t' | $value eq '#T') {
                $value = 1;
            }
            $keyword =~ tr/-A-Z/_a-z/;
            $contents{$keyword} = $value;
            $last_keyword = $keyword;
        }
    }
    return %contents;
}

