#
#   written by Michael Barton 18 March 2004 
#
#	adds tcltkgrass module support to d.rast.rescale
#
#--------------------------------------------------

if test "$GISBASE" = ""; then
 echo "You must be in GRASS GIS to run this program." >&2
 exit 1
fi   
     

for i do
	case $i in
		map=*)
			map=`echo $i | sed s/map=//` ;;
		output=*)
			output=`echo $i | sed s/output=//` ;;
		*)
			echo ""
			echo "Unrecognized option: $i"
			echo Options: map=raster_name output=raster_name
			exit
	esac
done



#run d.rast.rescale script

d.rast.rescale $map $output
