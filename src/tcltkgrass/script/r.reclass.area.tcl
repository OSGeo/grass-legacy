#
#   written by Michael Barton 18 March 2004 
#
#	adds tcltkgrass module support to r.reclass.area
#
#--------------------------------------------------

if test "$GISBASE" = ""; then
 echo "You must be in GRASS GIS to run this program." >&2
 exit 1
fi   
     

for i do
	case $i in
		gtlt=*)
			gtlt=`echo $i | sed s/gtlt=//` ;;
		area=*)
			area=`echo $i | sed s/area=//` ;;
		map=*)
			map=`echo $i | sed s/map=//` ;;
		output=*)
			output=`echo $i | sed s/output=//` ;;
		*)
			echo ""
			echo "Unrecognized option: $i"
			echo Options: gtlt=[gt|lt] area=value map=raster_name output=raster_name
			exit
	esac
done



#run r.reclass.area script

r.reclass.area -$gtlt $area $map $output
