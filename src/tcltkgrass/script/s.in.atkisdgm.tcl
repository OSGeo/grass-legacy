#
#   written by Michael Barton 18 March 2004 
#
#	adds tcltkgrass module support to s.in.atkisdgm
#
#--------------------------------------------------

if test "$GISBASE" = ""; then
 echo "You must be in GRASS GIS to run this program." >&2
 exit 1
fi   
     

for i do
	case $i in
		dgmfile=*)
			dgmfile=`echo $i | sed s/dgmfile=//` ;;
		*)
			echo ""
			echo "Unrecognized option: $i"
			echo Options: dgmfile=file
			exit
	esac
done



#run s.in.atkisdgm script

s.in.atkisdgm $dgmfile
