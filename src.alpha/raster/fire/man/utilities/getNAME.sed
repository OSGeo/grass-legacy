#	getNAME.sed
#
#	sed(1) script used to clean up the output from /usr/lib/getNAME
#	and make it usable for the GRASS permuted index.  This sed script
#	is invoked by the "make-permuted-index" script in this directory.
#
#	Amit Parghi
#	1992 12 07
#
#
#	Strip off the .TH at the front of the line
s/^\.TH \([a-z0-9\.]*\)/\1/
#
#	Remove the trailing .br .I "(GRASS Blah Blah)"
s/[ ]*\.br \.I[() A-Za-z0-9\."-]*$//
s/[ ]*\.br \\fI[() A-Za-z0-9\."-]*\\fR[ ]*$//
#
#	Remove the repetition of the program/section name.
s/\\fI[()a-z0-9A-Z\. 	]*\\fR//g
#
#	Remove any occurrences of \-
s/\\-//g
#
#	Remove trailing periods
s/\.$//
