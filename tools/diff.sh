#!/bin/bash

srcdir1=${1:-/usr/src/grass5-rel}
srcdir2=${2:-/usr/src/grass5-head}
dstdir=${3:-$HOME/grass/diff}

regex='\$\(Author\|Date\|Header\|Id\|Name\|RCSfile\|Revision\|Source\|State\):'

tmpfile=$dstdir/rel-head-1.txt
outfile=$dstdir/rel-head-2.txt
only1=$dstdir/only-1.txt
only2=$dstdir/only-2.txt
differ=$dstdir/diff.txt
files1=$dstdir/files-1.txt
files2=$dstdir/files-2.txt
dirs1=$dstdir/dirs-1.txt
dirs2=$dstdir/dirs-2.txt
gmakefiles2=$dstdir/files-2-g.txt
otherfiles2=$dstdir/files-2-o.txt
thediff=$dstdir/diff.diff

mkdir $dstdir 2>/dev/null

diff -rq -I $regex $srcdir1 $srcdir2 > $tmpfile

sed -e '/\/CVS/d' \
    -e 's!: !/!' \
    -e "s!^\(Only in $srcdir1\)/!\1: !" \
    -e "s!^\(Only in $srcdir2\)/!\1: !" \
    -e "s!Files $srcdir1/\(.*\) and $srcdir2/\(.*\) differ!Files differ: \2!" \
    $tmpfile | sort > $outfile

sed -n "s!Only in ${srcdir1}: !!p" $outfile > $only1
sed -n "s!Only in ${srcdir2}: !!p" $outfile > $only2
sed -n "s!Files differ: !!p" $outfile > $differ

> $files1
> $dirs1
while read file ; do
    if [ -f $srcdir1/$file ] ; then echo $file >> $files1 ; fi
    if [ -d $srcdir1/$file ] ; then echo $file >> $dirs1 ; fi
done < $only1

> $files2
> $dirs2
while read file ; do
    if [ -f $srcdir2/$file ] ; then echo $file >> $files2 ; fi
    if [ -d $srcdir2/$file ] ; then echo $file >> $dirs2 ; fi
done < $only2

fgrep "/Gmakefile" $files2 > $gmakefiles2
fgrep -v "/Gmakefile" $files2 > $otherfiles2

> $thediff
while read file ; do
    diff -u -I $regex $srcdir1/$file $srcdir2/$file >> $thediff
done < $differ
