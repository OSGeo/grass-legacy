#
#   written by Michael Barton 18 March 2004 
#
#	adds tcltkgrass module support to r.regression.line
#
#--------------------------------------------------

if test "$GISBASE" = ""; then
 echo "You must be in GRASS GIS to run this program." >&2
 exit 1
fi   
     

for i do
	case $i in
		rasterx=*)
			rasterx=`echo $i | sed s/rasterx=//` ;;
		rastery=*)
			rastery=`echo $i | sed s/rastery=//` ;;
		output=*)
			output=`echo $i | sed s/output=//` ;;
		*)
			echo ""
			echo "Unrecognized option: $i"
			echo Options: rasterx=raster_name rastery=raster_name output=results_file
			exit
	esac
done



#run r.regression.line script

r.regression.line $rasterx $rastery $output
