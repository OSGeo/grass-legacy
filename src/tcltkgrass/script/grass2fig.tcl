#
#   written by Michael Barton 18 March 2004 
#
#	adds tcltkgrass module support to grass2fig
#
#--------------------------------------------------

if test "$GISBASE" = ""; then
 echo "You must be in GRASS GIS to run this program." >&2
 exit 1
fi   
     

for i do
	case $i in
		vectascii=*)
			vectascii=`echo $i | sed s/vectascii=//` ;;
		figout=*)
			figout=`echo $i | sed s/figout=//` ;;
		*)
			echo ""
			echo "Unrecognized option: $i"
			echo Options: vectascii=ascii_vector figout=file
			exit
	esac
done



#run grass2fig script

grass2fig $vectascii > $figout
