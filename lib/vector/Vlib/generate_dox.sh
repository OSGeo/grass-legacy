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
cat $i.c | grep '\\fn '| sed '/^ *\\fn .*\(Vect_[a-zA-Z0-9_]*\).*$/s//\1/p' |sort -u | sed 's/$/();\
/g' >> $i.dox

#write footer:
echo "@}
*/"					>> $i.dox

done
