
# Remove these if they exist from last edit session
rm -f TAPE7.DAT BNOISEIN.DAT
rm -f TEMP fort.13 fort.15 fort.20 TAPE1.DAT out
rm -f TAPE2.DAT TAPE8.DAT TAPE3.DAT TAPE4.DAT TAPE55.DAT
rm -f TAPE70.DAT TAPE75.DAT fort.55

trap 'rm -f \
TAPE7.DAT BNOISEIN.DAT TEMP fort.13 fort.15 fort.20 TAPE1.DAT TAPE2.DAT \
out TAPE8.DAT TAPE3.DAT TAPE4.DAT TAPE55.DAT TAPE70.DAT TAPE75.DAT fort.55 ;\
exit' 1 2 3 4 5 6 7 8 10 12

# Run the editor
$EDITOR

# Run the main bnoise program
$LCDN

t=0
while [ $t = 0 ]
do
    echo ""
    echo "Enter name of file where results will be saved for noise2cell"
    echo "> \c"

    read request
    set - $request
    if [ $1 ]
    then
	touch $1
	if [ -w $1 ]
	then
	    rm $1
	    echo "Results will be saved in <$1>."
	    echo "Is this OK ?"
	    echo "> \c"
	    read request
	    case $request in
		    yes) t=1 ;;
		    y) t=1 ;;
		    YES) t=1 ;;
		    Y) t=1 ;;
		    *) ;;
	    esac
	fi
    fi
done

mv TAPE1.DAT $1
