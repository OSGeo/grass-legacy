#!/bin/sh

# dirty trick to generate module-pages for doxygen:
# Markus Neteler 10/2002

echo "Extracting all Vect_*() functions into local context pages..."

#get list of all C files:
LIST=`ls -1 *.c | sed 's/.c$//g'`

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
