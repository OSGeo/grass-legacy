:
####################################################################################
####    Script to calculate thicken a rasterised line using rooks case joins.   ####
####    Jo Wood, v1.0 Deceber, 1991, v1.2 March 1992, v2.0 July 1995.           ####
####                                                                            ####                
####################################################################################


#### Check that correct number of arguments have been given.

if test "$1" = "" || test "$2" != ""
then
        echo "Usage: $0 <dem_name>" >&2
        exit 1
fi

#### Check that user is running GRASS

if test "$GISRC" = ""
then
	echo "You must be running GRASS to execute $0" >&2
	exit 1
fi

#### Check GRASS variables are set

eval `g.gisenv`
: ${GISBASE?} ${GISDBASE?} ${LOCATION_NAME?} ${MAPSET?}

#### Envoke r.mapcalc to do the calculations

# Thickens rasterised contours by looking for diagonal cells.
# Look at local 2x3 neighbourhood and label cells

ul=$1[-1,-1]
um=$1[-1,0]
ur=$1[-1,1]
ll=$1[0,-1]
lm=$1
lr=$1[0,1]

r.mapcalc <<EOF
	$lm= if( ($lm==0) && ($um !=0) && ((($ul==0) && ($um==$ll)) || \
		 	      		   (($ur==0) && ($um==$lr))),$um,$lm)
EOF
