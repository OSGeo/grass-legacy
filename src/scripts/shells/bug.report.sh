:

BUG_PERSON=neteler@geog.uni-hannover.de

if [ $# -lt 1 ] || [ "$1" == "help"  ]
then
	cmd=`echo $0 | sed -e 's:.*/::'`
	echo Usage: $cmd command.name [command arguments]
	exit
fi

if [ ! -x $GISBASE/bin/$1 ]
then
	echo ""
	cmd=`echo $0 | sed -e 's:.*/::'`
	echo Sorry, $1 is not a GRASS command.  Therefore no bug!
	echo Usage: $cmd command.name [command arguments]
	exit
fi

o=/tmp/bug$$
rm -f $o
trap 'rm -f $o $o.1 $o.2 ; exit' 1 2 3 15
touch $o

echo "" >> $o
echo -n "GRASS Bug Report, version " >> $o
g.version >> $o
echo "" >> $o
echo -n "Program: $1  " >> $o
cmd=0
inter=0
if [ -x $GISBASE/etc/bin/main/cmd/$1 ]
then
	echo ' (main-cmd)' >> $o
	cmd=1
fi
if [ -x $GISBASE/etc/bin/alpha/cmd/$1 ]
then
	echo ' (alpha-cmd)' >> $o
	cmd=1
fi
if [ -x $GISBASE/etc/bin/contrib/cmd/$1 ]
then
	echo ' (contrib-cmd)' >> $o
	cmd=1
fi
if [ -x $GISBASE/etc/bin/contrib/inter/$1 ]
then
	echo ' (contrib-inter)' >> $o
	inter=1
fi
if [ -x $GISBASE/etc/bin/main/inter/$1 ]
then
	echo ' (main-inter)' >> $o
	inter=1
fi
if [ -x $GISBASE/etc/bin/alpha/inter/$1 ]
then
	echo ' (alpha-inter)' >> $o
	inter=1
fi
if [ 0 = $inter -a 0 = $cmd ]
then
	echo ' (binary only)' >> $o
fi
echo '----------------------------------------------------------------------------- ' >> $o
echo $* >> $o
echo '----------------------------------------------------------------------------- ' >> $o
echo "" >> $o

echo -n "        Date: " >> $o
date >> $o
echo "" >> $o
echo -n " Tester Name: " >> $o
who am i >> $o
echo -n "Machine Info: " >> $o
uname -a >> $o
echo "     GISBASE: $GISBASE" >> $o

echo "" >> $o.1 >> $o
echo -n "   Directory: " >> $o.1
echo $GISDBASE >> $o.1
echo -n "    Database: " >> $o.1
echo $LOCATION_NAME >> $o.1
echo -n "      Mapset: " >> $o.1
echo $MAPSET >> $o.1

rm -f $o.2
g.region -p > $o.2

pr -m -t -l1 $o.1 $o.2 | expand >> $o
rm $o.1 $o.2

if [ 1 = $cmd ]
then
echo '----------------------------------------------------------------------------- ' >> $o
$1 help 2>&1 | \
	sed -e '1,/^$/d'  \
		-e '/default:/d' \
		-e '/options:/d' \
		-e '/Flags:/s/$/             Test    Comments/' \
		-e '/Parameters:/s/$/        Test    Comments/' \
		-e '/^ /s/\( *\)\([^ ]* *\).*/                    \2/' \
		-e '/,/d' \
		-e '/ $/s/.*\(...................\)./\1____   ___________________________________________________/' >> $o
echo '----------------------------------------------------------------------------- ' >> $o
fi

echo "" >> $o

echo "Contact information" >> $o
echo "             Name:" >> $o
echo "     Organization:" >> $o
echo "          Address:" >> $o
echo "                 :" >> $o
echo "            Phone:" >> $o
echo "           E-mail:" >> $o

echo "" >> $o
echo "Abstract of bug (brief, one-line description):" >> $o
echo "" >> $o
echo "" >> $o
echo "" >> $o

echo "Description of bug (be as specific as possible in this section):" >> $o

${EDITOR-vi} $o

clear
need_answer=1
while test $need_answer -eq 1
do
	echo ""
	echo "Should this report be (choose one):"
	echo " 1) mailed to $BUG_PERSON ."
	echo " 2) added to the file $HOME/grass.bugs ."
	echo " 3) both 1 and 2."
	echo " 4) thrown away."
	echo -n "> "

	read answer
	need_answer=0
	case $answer in
		1) cat $o | sed 's/~/ /' | mail $BUG_PERSON ;
			echo Your report has been mailed, Thank you ;;
		2) touch $HOME/grass.bugs ; cat $o >> $HOME/grass.bugs ;
			echo Your report has been saved, Thank you ;;
		3) cat $o | sed 's/~/ /' | mail $BUG_PERSON ;
			touch $HOME/grass.bugs ; cat $o >> $HOME/grass.bugs ;
			echo Your report has been mailed and saved, Thank you ;;
		4) ;;
		*) need_answer=1; echo Please enter a number;;
	esac
done

rm $o
