
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
	
	float rules_man_n(land_use_label)

	To assign the mannings 'n' values based on the landuse condition
	and returns the corresponding mannings 'n' value 
	recommendaded by the AGNPS manual.
*/

#include "agnps_input.h"


float rules_man_n(land_use_label)
int	land_use_label;

{

	if (fallow == land_use_label) return(0.038);

	else if (row_crops == land_use_label) return(0.075);

	else if ((small_grain == land_use_label) ||
		(legumes == land_use_label) ||
		(rotation_meadow == land_use_label) ||
		(close_seeded_legumes == land_use_label))
			return(0.30);

	else if ((pasture == land_use_label) 
		  || (range == land_use_label))
			return(0.08);

	else if ((meadow == land_use_label) || 
		(permanent_meadow == land_use_label)) 
			return(0.59);

	else if ((woods == land_use_label) || 
		 (woodland == land_use_label))
			return(0.29);

	else if (grass_waterway == land_use_label) 
			return(0.60);

	else if ((hard_surface == land_use_label) ||
		 (farmsteads == land_use_label) ||
		 (roads_(dirt) == land_use_label) ||
		 (urban == land_use_label))
			return(0.01);

	else if ((water == land_use_label) ||
		 (marsh == land_use_label))
			return(0.990);

	else
	{
	   fprintf (stderr,"The %d is not defined!!\n Please reclassify the map and refer AGNPS user manaual for proper mannings 'n' \n",land_use_label);
	   clean_up();
	   exit(0);
	}
}

