#
#   written by Michael Barton 18 March 2004 
#
#	adds tcltkgrass module support to fig2grass
#
#--------------------------------------------------

if test "$GISBASE" = ""; then
 echo "You must be in GRASS GIS to run this program." >&2
 exit 1
fi   
     

for i do
	case $i in
		figfile=*)
			figfile=`echo $i | sed s/figfile=//` ;;
		vectascii=*)
			vectascii=`echo $i | sed s/vectascii=//` ;;
		*)
			echo ""
			echo "Unrecognized option: $i"
			echo Options: figfile=file vectascii=ascii_vector 
			exit
	esac
done



#run grass2fig script

fig2grass $figfile > $vectascii
