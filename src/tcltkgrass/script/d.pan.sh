#!/bin/sh

# region selection

d.pan $*

# graphic monitor must be erased unless only help is required

for arg do
    if [ $arg = help ] ; then exit; fi
done

d.erase
