#
#   written by Michael Barton 18 March 2004 
#
#	adds tcltkgrass module support to d.siter2
#
#--------------------------------------------------

if test "$GISBASE" = ""; then
 echo "You must be in GRASS GIS to run this program." >&2
 exit 1
fi   
     

# evaluate arguments
for i do
	case $i in
		sites=*)
			sites=`echo $i | sed s/sites=//` ;;
		*)
			echo ""
			echo "Unrecognized option: $i"
			echo "Options: sites=site_list"
			exit
	esac
done



#run d.monsize script

d.siter $sites
