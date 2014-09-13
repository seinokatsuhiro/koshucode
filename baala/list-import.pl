#!/usr/bin/env perl
#
#  DESCRIPTION
#    List import lines.
#

use strict;

my @imp = ();
my $cnt = 0;

#
#  Collect imports
#

foreach my $path ( @ARGV ) {
    open (IN, "< $path") || die "Couldn't open $path!\n";

    my $mod = "<Unknown>";
    while ( <IN> ) {
        chomp;
        if ( $mod eq "<Unknown>" && /^module\s+(.*)/ ) {
            $mod = $1;
        } elsif ( /^\s*import(\s+qualified)?\s+([^ ]+)(\s+as\s+.*)?/ ) {
            my $line = sprintf "|-- IMPORT  /module %-42s  /import %s",
                quote ($mod), quote ($2);
            push @imp, $line;
            $cnt++;
        }
    }
    push @imp, "";

    close (IN);
}

sub quote {
    return "\"$_[0]\"";
}

#
#  Output
#

print <<EOS;
** -*- koshu -*-
**
**  DESCRIPTION
**    List of module imports.
**
**  IMPORT
**    <<< Module named /module imports module /import. >>>
**
**  SUMMARY
**    $cnt judges
**

EOS

foreach my $line (@imp) {
    print $line;
    print "\n";
}

