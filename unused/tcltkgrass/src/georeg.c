#include <stdio.h>
#include "viewstruct.h"
#include "gis.h"

SetGeographicRegion(region, north, south, west, east, nsRes, ewRes)
double north,south,west,east,nsRes,ewRes;
GEOREG *region;
{
	region->northEastCoordinate.x = east;
	region->northEastCoordinate.y = north;
	region->southWestCoordinate.x = west;
	region->southWestCoordinate.y = south;
    region->northSouthResolution = nsRes;
    region->eastWestResolution = ewRes;
}

conversions(region,x,y,width,height)
int x,y,width,height;
GEOREG *region;
{
	double A_hori_to_vert ;
	double D_vert, D_hori ;
	double WIND_BOT, WIND_TOP, WIND_LEFT, WIND_RITE ;
	double Dadj ;

	x = 0;
	y = 0;
	WIND_TOP  = (double) y;
	WIND_BOT  = (double) (y + height - 1) ;
	WIND_LEFT = (double) x;
	WIND_RITE = (double) (x + width - 1);

	region->ns_resolution = region->northSouthResolution;
	region->ew_resolution = region->eastWestResolution;


	/* Key all coordinate limits off UTM window limits  */

	region->U_west  = region->southWestCoordinate.x;
	region->U_east  = region->northEastCoordinate.x;
	region->U_south = region->southWestCoordinate.y;
	region->U_north = region->northEastCoordinate.y;

	/* Calculate Array window limits from UTM limits */

	region->ARRAY_COLS = (int) (region->U_east - region->U_west + region->ew_resolution/2) / region->ew_resolution ;
	region->ARRAY_ROWS = (int) (region->U_north - region->U_south + region->ns_resolution/2) / region->ns_resolution ;

	region->A_west = 0.0 ;
	region->A_east = (region->U_east - region->U_west) / region->ew_resolution ;
	region->A_south = (double)(region->ARRAY_ROWS) ;
	region->A_north = (double)(region->ARRAY_ROWS) - (region->U_north - region->U_south) / region->ns_resolution ;
	A_hori_to_vert = region->ew_resolution / region->ns_resolution ;

	/* Calculate Dot limits from Array limits */

	D_vert = WIND_BOT - WIND_TOP ;
	D_hori = WIND_RITE - WIND_LEFT ;

	region->D_north = WIND_TOP ;
	region->D_west  = WIND_LEFT ;

	region->A_to_D_xconv = D_hori / ( (region->A_east  - region->A_west ) * A_hori_to_vert) ;
	region->A_to_D_yconv = D_vert / (region->A_south - region->A_north) ;

	if (region->A_to_D_xconv > region->A_to_D_yconv)
		region->A_to_D_xconv = region->A_to_D_yconv ;
	else
		region->A_to_D_yconv = region->A_to_D_xconv ;

	region->A_to_D_xconv *= A_hori_to_vert ;

	D_hori = region->A_to_D_xconv * (region->A_east  - region->A_west ) ;
	D_vert = region->A_to_D_yconv * (region->A_south - region->A_north) ;

	/* Pull all edges in so picture stays centered */

	Dadj = ((WIND_BOT - WIND_TOP ) - D_vert) / 2 ;
	if (Dadj > 0.0)
	{
	    region->D_north = WIND_TOP + Dadj ;
	    region->D_south = region->D_north + D_vert ;
	}
	else
	{
		region->D_south = WIND_BOT ;
	}
	Dadj = ((WIND_RITE - WIND_LEFT ) - D_hori) / 2 ;
	if (Dadj > 0.0)
	{
	    region->D_west = WIND_LEFT + Dadj ;
	    region->D_east = region->D_west + D_hori ;
	}
	else
	{
		region->D_east = WIND_RITE ;
	}

	region->U_to_D_xconv = (region->D_east  - region->D_west ) / (region->U_east  - region->U_west ) ;
	region->U_to_D_yconv = (region->D_south - region->D_north) / (region->U_north - region->U_south) ;

}


double D_u_to_a_row(region,U_row) 
GEOREG *region;
double U_row;
{ 
     return( (double) region->ARRAY_ROWS - ( ( (double) U_row - region->U_south) / region->ns_resolution ) ) ;
}

double D_u_to_a_col(region,U_col) 
GEOREG *region;
double U_col;
{ 
     return( ( (double) U_col - region->U_west ) / region->ew_resolution )  ; 
}

double D_a_to_d_row(region,A_row) 
GEOREG *region;
double A_row;
{ 
     return( (A_row - region->A_north) * region->A_to_D_yconv + region->D_north);
}

double D_a_to_d_col(region, A_col) 
GEOREG *region;
double A_col;
{ 
     return( (A_col - region->A_west) * region->A_to_D_xconv + region->D_west) ; 
}

double D_d_to_u_row(region,D_row) 
GEOREG *region;
double D_row;
{ 
     return( region->U_north - ( (double) (D_row) - region->D_north) / region->U_to_D_yconv ) ;
}

double D_d_to_u_col(region,D_col) 
GEOREG *region;
double D_col;
{ 
     return(region->U_west + ( (double) (D_col) - region->D_west) / region->U_to_D_xconv ) ;
}

double D_d_to_a_row(region,D_row) 
GEOREG *region;
double D_row;
{ 
     return( ( (double) D_row - region->D_north) / region->A_to_D_yconv + region->A_north ); 
}

double D_d_to_a_col(region,D_col) 
GEOREG *region;
double D_col;
{ 
     return( ( (double) D_col - region->D_west ) / region->A_to_D_xconv + region->A_west ) ; 
}

getdefregion(aregion)
GEOREG *aregion;
{
	struct Cell_head defregion;
	
	G_get_default_window(&defregion);
	
	aregion->northEastCoordinate.y = defregion.north;
	aregion->southWestCoordinate.y = defregion.south;
	aregion->northEastCoordinate.x = defregion.east;
	aregion->southWestCoordinate.x = defregion.west;
	aregion->northSouthResolution  = defregion.ns_res;
	aregion->eastWestResolution    = defregion.ew_res;
}

	

	




