#!/bin/sh
#
# Creates a new man page from template or existing page
#

ok=n
while [ $ok = n ]
  do
    echo -n 'name of program : '
    read manpagename
    outputfile="./html/$manpagename.html"
    echo
    echo -n "Creation of file $outputfile [y/n] ? "
    read r
    if [ "$r" == "y" ]
      then
        ok=y
    fi
  echo
done
ok=n
while [ $ok = n ]
  do
    echo 'If you want to use the man page of an existing program as template,'
    echo 'give the name of the existing program. If you want to create the'
    echo -n 'page from a template, just hit <ENTER> : '
    read inputname
    if [ -z "$inputname" ]
      then
        rule="-e \"s/_manpagename_/$manpagename/\""
        inputfile="./template.ht"
	echo
	echo "Enter a one line description of the command :"
	read manpagedescr
        rule="$rule -e \"s/_manpagedescr_/$manpagedescr/\""
	echo "In which category do you place your command ?"
	echo "    0 no category indication" 
	echo "    1 \"GRASS Database Program\""
	echo "    2 \"GRASS Raster Program\"" 
	echo "    3 \"GRASS Vector Program\"" 
	echo "    4 \"GRASS Sites Program\"" 
	echo "    5 \"GRASS Imagery Program\"" 
	echo "    your own category string" 
	echo
	echo -n "Your choice : "
	read rep
	case rep in
	  1)	manpageplace="GRASS Database Program";;
	  2)	manpageplace="GRASS Raster Program";;
	  3)	manpageplace="GRASS Vector Program";;
	  4)	manpageplace="GRASS Sites Program";;
	  5)	manpageplace="GRASS Imagery Program";;
	  *)	manpageplace="$rep";;
	esac
	if [ -n "$rep" -a "$rep" != "0" ]
	  then
            rule="$rule -e \"s/_manpageplace_/$manpageplace/\""
	  else
	    rule="$rule -e '/_manpageplace_/d'"
	fi
	echo -n "Name of the author : "
	read manpageauthor
        rule="$rule -e \"s/_manpageauthor_/$manpageauthor/\""
	echo -n "Mail of the author (optional) : "
	read authormail
	if [ -z "$authormail" ]
	  then
            rule="$rule -e \"s/(.*_manpageauthormail_.*)//\""
	  else
            rule="$rule -e \"s/_manpageauthormail_/$authormail/g\""
	fi
	ok=y
      else
        rule="-e \"s/$inputname/$manpagename/ig\""
        inputfile="./html/$inputname.html"
        if [ ! -f "$inputname" ]
          then
            echo "Cannot find $inputname - Try again"
	    inputname=
	  else
	    ok=y
        fi
    fi
done
eval sed $rule < $inputfile > $outputfile
echo
echo "$outputfile created.  Now it's time to edit it."
echo "What editor do you want to use (netscape -edit, gvim, emacs, ... ) ?"
echo -n "Enter your choice [default: ${EDITOR-netscape -edit}] : "
editor="${EDITOR-netscape -edit}"
read choosededitor
if [ -z "$choosededitor" ]
  then
    editor="${EDITOR-netscape -edit}"
  else
    editor="$choosededitor"
fi
if [ "`basename $editor`" = "netscape" ]
  then
    outputfile="file:`pwd`/html/$manpagename.html"
fi
echo executing $editor $outputfile
exec $editor $outputfile
