#!/bin/sh

# what to do in case of user break:
function exitprocedure()
{
 echo "User break!"
 #cleanup
 exit 1
}

# shell check for user break (signal list: trap -l)
trap "exitprocedure" 2 3 9 15

if test "$GISBASE" = ""; then
echo "You must be in GRASS to run this program."
exit
fi

if [ $# -eq 0 -o "$1" = "-help" -o "$1" = "help" -o "$1" = "-h" ]
then
 echo "Description:"
 echo ""
 echo "Imports contour levels and master contour levels in DXF file"
 echo "format to GRASS vector format."
 echo ""
 echo "Usage: v.in.dxf3d.sh dxf=name line1=name line2=name"
 exit
fi

# evaluate arguments
for i do
	case $i in
		dxf=*)
			DXF=`echo $i | sed s/dxf=// | sed 's/\.dxf$//'` ;;
		line1=*)
			LINE1=`echo $i | sed s/line1=//` ;;
		line2=*)
			LINE2=`echo $i | sed s/line2=//` ;;
		*)
			echo ""
			echo "Unrecognized option: $i"
			echo Options: dxf=name line1=name line2=name
			exit
	esac
done

# execute conversion 
v.in.dxf dxf=$DXF.dxf lines=$LINE1,$LINE2
v.in.dxf3d dxf=$DXF.dxf lines=$LINE1,$LINE2

v.support $DXF.$LINE1
v.support $DXF.$LINE2

