#!/bin/sh

# region selection

g.region $* -p

# graphic monitor must be erased unless:
# - only help is required
# - or option -u is present (no update)

for arg do
    if [ $arg = -u -o $arg = help ] ; then exit; fi
done

d.erase
