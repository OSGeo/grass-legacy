#
#   written by Michael Barton 18 March 2004 
#
#	adds tcltkgrass module support to r.to.pg
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
		pgtable=*)
			pgtable=`echo $i | sed s/pgtable=//` ;;
		*)
			echo ""
			echo "Unrecognized option: $i"
			echo Options: map=raster_name pgtable=postgresql_tablename
			exit
	esac
done



#run r.to.pg script

r.to.pg $map $pgtable
