#!/usr/bin/perl

$maxcat = $ARGV[$0];
print "$maxcat\n";
#
# Create the displayrules file where you are executing psu_agnps    
# The environment variable $EXECDIR was set in psu_agnps_max_cat.sh
# which called this script, make_display_rules.pl
#
$rulefile = "$ENV{'EXECDIR'}/displayrules"; 
 
$bound1 = int($maxcat * 0.25);
$bound1label = $bound1/100.0;
$bound2 = int($maxcat * 0.50);
$bound2label = $bound2/100.0;
$bound3 = int($maxcat * 0.75);
$bound3label = $bound3/100.0;

# Open the rule file and write the reclass rules to it.

open(RULEFILE,">$rulefile") || die "Error attempting to open new rulefile\n";
print RULEFILE "0 = 0 nodata\n";
printf RULEFILE "%s %s %s %s %.2f %s","1 thru ",$bound1," = 1 ", "0.01 -",$bound1label,"\n"; 
printf RULEFILE "%s %s %s %s %.2f %s %.2f %s",$bound1+1," thru ",$bound2," = 2 ",$bound1label+.01,"-",$bound2label,"\n"; 
printf RULEFILE "%s %s %s %s %.2f %s %.2f %s",$bound2+1," thru ",$bound3," = 3 ",$bound2label+.01,"-",$bound3label,"\n"; 
printf RULEFILE "%s %s %s %s %.2f %s %.2f %s",$bound3+1," thru ",$maxcat," = 4 ",$bound3label+.01,"-",$maxcat/100.0,"\n"; 
close(RULEFILE);
