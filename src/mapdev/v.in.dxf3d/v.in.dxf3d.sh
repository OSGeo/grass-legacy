#!/bin/sh
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
