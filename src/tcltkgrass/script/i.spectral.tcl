# Michael Barton
#
# i.spectral.tcl v.1 2004/03/17 01:04:51 M. Barton
# gives tcltk module support to i.spectral script
# 

if test "$GISBASE" = ""; then
echo "You must be in GRASS to run this program."
exit
fi


# evaluate arguments

for i do
	case $i in
		image=*)
			image=`echo $i | sed s/image=//` ;;
		*)
	esac
done

# replace commas with spaces

image=`echo $image | sed s/,/" "/`;


#run i.spectral script

i.spectral $image
