
	/*---------------------------------------------------------*
	 *               AGNPS/GRASS Interface Project             *
	 *  Developed in the Agriculture Engineering Department    *
	 *                at Purdue University                     *
	 *                        by                               *
	 *         Raghavan Srinivasan and Bernard Engel           *
	 *                                                         *
	 *   (c)Copyright, 1992 Purdue Research Foundation, West   *
	 *   Lafayette, Indiana 47907. All Rights Reserved. Unless *
	 *   permission is granted, this material shall not be     *
	 *   copied, reproduced or coded for reproduction by any   *
	 *   electrical, mechanical or chemical processes,  or     *
	 *   combinations thereof, now known or later developed.   *
	 *---------------------------------------------------------*/

/*	July, 1991  Agricultural Engineering, Purdue University
	Raghavan Srinivasan (srin@ecn.purdue.edu)
	
	int rules_cod(land_use_label)

	To assign the COD values based on the landuse condition
	and returns the corresponding COD value 
	recommendaded by the AGNPS manual (pp 20).
*/

#include "agnps_input.h"


int rules_cod(land_use_label)
int	land_use_label;

{


	if (fallow == land_use_label) return(115);

	else if (row_crops == land_use_label) return(170);

	else if ((small_grain == land_use_label) ||
		(legumes == land_use_label) ||
		(rotation_meadow == land_use_label) ||
		(close_seeded_legumes == land_use_label))
			return(80);

	else if ((pasture == land_use_label)
		  || (range == land_use_label))
			return(60);

	else if ((meadow == land_use_label) || 
		(permanent_meadow == land_use_label))
			return(20);

	else if ((woods == land_use_label) || 
		 (woodland == land_use_label))
			return(65);

	else if (grass_waterway == land_use_label) 
			return(20);

	else if ((hard_surface == land_use_label) ||
		 (farmsteads == land_use_label) ||
		 (roads_(dirt) == land_use_label) ||
		 (urban == land_use_label))
			return(80);

	else if (water == land_use_label) return(0);
	else if (marsh == land_use_label) return(25);

	else
	{
	   fprintf (stderr,"The %d is not defined!!\n Please reclassify the map and refer AGNPS user manaual for proper COD factor \n",land_use_label);
	   clean_up();
	   exit(0);
	}
}

