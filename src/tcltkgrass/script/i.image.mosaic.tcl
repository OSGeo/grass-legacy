#!/bin/sh
#   written by Michael Barton 17 March 2004 
#
#	adds tcltkgrass module support to i.mosaic
#
#--------------------------------------------------

if test "$GISBASE" = ""; then
 echo "You must be in GRASS GIS to run this program." >&2
 exit 1
fi   
     

for i do
	case $i in
		image=*)
			image=`echo $i | sed s/image=//` ;;
		*)
	esac
done

# replace commas with spaces

image=`echo $image | sed s/,/" "/`;


#run i.spectral script

i.image.mosaic $image
