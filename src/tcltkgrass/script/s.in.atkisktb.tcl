#
#   written by Michael Barton 18 March 2004 
#
#	adds tcltkgrass module support to s.in.atkisktb
#
#--------------------------------------------------

if test "$GISBASE" = ""; then
 echo "You must be in GRASS GIS to run this program." >&2
 exit 1
fi   
     

for i do
	case $i in
		ktbfile=*)
			ktbfile=`echo $i | sed s/ktbfile=//` ;;
		*)
			echo ""
			echo "Unrecognized option: $i"
			echo Options: ktbfile=file
			exit
	esac
done



#run s.in.atkisktb script

s.in.atkisktb $ktbfile
