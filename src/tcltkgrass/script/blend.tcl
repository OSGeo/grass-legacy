#
#   written by Michael Barton 18 March 2004 
#
#	adds tcltkgrass module support to blend.sh
#
#--------------------------------------------------

if test "$GISBASE" = ""; then
 echo "You must be in GRASS GIS to run this program." >&2
 exit 1
fi   
     

for i do
	case $i in
		rast1=*)
			rast1=`echo $i | sed s/rast1=//` ;;
		rast2=*)
			rast2=`echo $i | sed s/rast2=//` ;;
		percent=*)
			percent=`echo $i | sed s/percent=//` ;;
		outbase=*)
			outbase=`echo $i | sed s/outbase=//` ;;
		*)
			echo ""
			echo "Unrecognized option: $i"
			echo Options: rast1=raster_name rast2=raster_name percent=value outbase=name 
			exit
	esac
done


#run blend.sh script

blend.sh $rast1 $rast2 $percent $outbase
