#
#   written by Michael Barton 18 March 2004 
#
#	adds tcltkgrass module support to d.monsize
#
#--------------------------------------------------

if test "$GISBASE" = ""; then
 echo "You must be in GRASS GIS to run this program." >&2
 exit 1
fi   
     

# evaluate arguments
for i do
	case $i in
		width=*)
			width=`echo $i | sed s/width=//` ;;
		height=*)
			height=`echo $i | sed s/height=//` ;;
		monitor=*)
			monitor=`echo $i | sed s/monitor=//` ;;
		*)
			echo ""
			echo "Unrecognized option: $i"
			echo "Options: width=monitor_width height=monitor_height monitor=monitor_name"
			exit
	esac
done



#run d.monsize script

d.monsize $width $height $monitor
