#!/bin/sh

# dirty trick to generate module-pages for doxygen:
# Markus Neteler 10/2002

# TODO: improve function name extraction, see below

echo "Extracting all Vect_*() functions into local context pages..."

#get list of all C files:
LIST=`ls -1 *.c | sed 's/.c$//g'`

#unused:
#get list of all C files, ignore *_something.c
#LIST=`ls -1 *.c | sed 's/.c$//g' | cut -d'_' -f1 | sort -u`

#build the doxygen module pages:

for i in $LIST
do

#write header for dox file:
echo "/*!
\defgroup $i Vector $i routines
@{"					> $i.dox

# extract function names starting with Vect_*():
# TODO: improve extraction of Vect_*() function names:
grep Vect_ $i* |grep -v Vect__ |grep '\\fn' | cut -d' ' -f4 | sed 's/$/();\
/g' >> $i.dox

#write footer:
echo "@}
*/"					>> $i.dox

done
