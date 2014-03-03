#!/usr/bin/env perl
#
#  DESCRIPTION
#    Count lines of Haskell source files.
#
#  NOTES
#    This script is stolen from count_lines.lprl in GHC,
#    and changed to report in koshucode.
#

use strict;

my %DirCode     = ();
my %ModCode     = ();
my %DirComments = ();
my %ModComments = ();
my %Basename    = ();
my %Dirname     = ();

#
#  Count sloc
#

foreach my $path ( @ARGV ) {

    open(IN, "< $path") || die "Couldn't open $path!\n";

    my $code = 0;
    while (<IN>) {
        s/--.*//;
        s/{-.*-}//;
        s/\/\/.*//;
        next if /^\s*$/;
        $code++;
    }

    close(IN);

    my $comments;
    my $wc = `wc $path`; die "wc failed: $path\n" if $? != 0;
    if ( $wc =~ /\s*(\d+)\s*(\d+)\s*(\d+)/ ) {
        $comments = $1 - $code;
    } else {
        die "Can't grok wc format: $wc";
    }

    if ( $path =~ /(.*)\/(.*)/ ) {
        my $dir  = $1;
        my $base = $2;
        $Dirname     {$path}   = $dir;
        $Basename    {$path}   = $base;
        $DirCode     {$dir}   += $code;
        $ModCode     {$path}  += $code;
        $DirComments {$dir}   += $comments;
        $ModComments {$path}  += $comments;
    } else {
        print STDERR "not counted in a directory: $path\n";
        $Dirname     {$path}   = $path;
        $Basename    {$path}   = $path;
        $ModCode     {$path}  += $code;
        $ModComments {$path}  += $comments;
    }
}

#
#  Report for directories
#

print "** -*- koshu -*-\n\n";

my $tot = 0;
my $totcmts = 0;

foreach my $d (sort (keys %DirCode)) {
    printf "|-- DIR  /code %-3d  /comment %-3d  /dir \"%s\"\n",
      $DirCode{$d}, $DirComments{$d}, $d;
    $tot     += $DirCode{$d};
    $totcmts += $DirComments{$d};
}

printf "\n|-- DIR-TOTAL  /code %d  /comment %d\n\n", $tot, $totcmts;

#
#  Report for files
#

$tot = 0;
$totcmts = 0;

foreach my $m (sort (keys %ModCode)) {
    my $base = "\"$Basename{$m}\"";
    printf "|-- FILE  /code %-3d  /comment %-3d  /file %-15s  /dir \"%s\"\n",
      $ModCode{$m}, $ModComments{$m}, $base, $Dirname{$m};
    $tot += $ModCode{$m};
    $totcmts += $ModComments{$m};
}

printf "\n|-- FILE-TOTAL  /code %d  /comment %d\n", $tot, $totcmts;

