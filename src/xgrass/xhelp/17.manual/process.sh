#!/bin/sh

for i in Help.pages/*
do
    if [ ! -d `dirname help.new/$i` ]; then
	echo making directory `dirname help.new/$i`
	mkdir -p `dirname help.new/$i`
    fi
    if [ -f $i ]; then
	echo processing $j
        /xgrass/src/processHelp < $i > help.new/$i
    else
	for j in $i/*
	do
	    if [ ! -d `dirname help.new/$j` ]; then
	        echo making directory `dirname help.new/$j`
		mkdir -p `dirname help.new/$j`
	    fi
	    echo processing $j
            /xgrass/src/processHelp < $j > help.new/$j
	done
    fi
done
