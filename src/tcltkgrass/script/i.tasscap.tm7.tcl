#
#   written by Michael Barton 18 March 2004 
#
#	adds tcltkgrass module support to i.tasscap.tm7
#
#--------------------------------------------------

if test "$GISBASE" = ""; then
 echo "You must be in GRASS GIS to run this program." >&2
 exit 1
fi   
     

for i do
	case $i in
		tm1=*)
			tm1=`echo $i | sed s/tm1=//` ;;
		tm2=*)
			tm2=`echo $i | sed s/tm2=//` ;;
		tm3=*)
			tm3=`echo $i | sed s/tm3=//` ;;
		tm4=*)
			tm4=`echo $i | sed s/tm4=//` ;;
		tm5=*)
			tm5=`echo $i | sed s/tm5=//` ;;
		tm7=*)
			tm7=`echo $i | sed s/tm7=//` ;;
		output=*)
			output=`echo $i | sed s/output=//` ;;
		*)
			echo ""
			echo "Unrecognized option: $i"
			echo Options: tm1=image tm2=image tm3=image tm4=image tm5=image tm7=image output=image
			exit
	esac
done



#run i.tasscap.tm7 script

i.tasscap.tm7 $tm1 $tm2 $tm3 $tm4 $tm5 $tm7 $output
