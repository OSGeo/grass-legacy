#
#   written by Michael Barton 18 March 2004 
#
#	adds tcltkgrass module support to v.out.xfig
#
#--------------------------------------------------

if test "$GISBASE" = ""; then
 echo "You must be in GRASS GIS to run this program." >&2
 exit 1
fi   
     

for i do
	case $i in
		vect=*)
			vect=`echo $i | sed s/vect=//` ;;
		*)
			echo ""
			echo "Unrecognized option: $i"
			echo Options: vect=vector_name
			exit
	esac
done



#run v.out.xfig script

v.out.xfig $vect
