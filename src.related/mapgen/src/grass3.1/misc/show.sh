
#!/bin/sh

if test $# = 0
then
	preview -d GRASS -m map.def ovm/* 
else
	if test "$1" = -m
		then
		preview -d GRASS $*
	else
		preview -d GRASS -m map.def $*
	fi

fi
