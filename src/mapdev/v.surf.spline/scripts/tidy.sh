:
####################################################################################
####    Script to remove files created by v.surf.spline				####
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

g.remove $1.1,$1.2,$1.3,$1.4,$1.contours
g.remove $1.1.weights,$1.2.weights,$1.3.weights,$1.4.weights
