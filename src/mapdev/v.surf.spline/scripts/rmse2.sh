:
####################################################################################
####    Script to calculate final interpolated DEM from four sets of profiles   ####
####    created by v.surf.spline. Also calculates the RMSE for each cell in DEM ####
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

# DEM is the weighted average of the four interpolated profiles. The weight of each profile
# depends on the distance to the nearest contours. The nearer to a contour line the greater
# the weight.
# RMSE is the weighted standard deviation of the four profile elevations.

r.mapcalc <<EOF
	$1.1.weights = if($1.1.weights >5000,0,$1.1.weights)
	$1.2.weights = if($1.2.weights >5000,0,$1.2.weights)
	$1.3.weights = if($1.3.weights >5000,0,$1.3.weights)
	$1.4.weights = if($1.4.weights >5000,0,$1.4.weights)

	$1.rmse = if ($1.contours,0,sqrt(						\
			((($1.1-$1)*($1.1-$1)*$1.1.weights*$1.1.weights) +  		\
			 (($1.2-$1)*($1.2-$1)*$1.2.weights*$1.2.weights) +  		\
		         (($1.3-$1)*($1.3-$1)*$1.3.weights*$1.3.weights) +  		\
			 (($1.4-$1)*($1.4-$1)*$1.4.weights*$1.4.weights)) / 		\
			(($1.1.weights*$1.1.weights) + ($1.2.weights*$1.2.weights) + 	\
			 ($1.3.weights*$1.3.weights) + ($1.4.weights*$1.4.weights))	\
					))
EOF
