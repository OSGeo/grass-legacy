
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
	
	float rules_sur_cond(land_use_label,mgmt_pr,hy_cond_label)

	To assign the surface condition constant based on
	the mgmt_pr, land_use and hy_cond labels in the corresponding
	maps and returns the corresponding surface condition constant
	recommendaded by the AGNPS manual.
*/

#include "agnps_input.h"


float rules_sur_cond(land_use_label,mgmt_pr,hy_cond_label)
char	*mgmt_pr, *hy_cond_label;
int	land_use_label;

{


	if (fallow == land_use_label) return(0.22);

	else if ((row_crops == land_use_label) && 
		  (strcmp("straight row",mgmt_pr) == 0))
			return(0.05);

	else if ((row_crops == land_use_label) && 
		 (strcmp("straight row",mgmt_pr) != 0))
			return(0.29);

	else if ((small_grain == land_use_label) ||
		(legumes == land_use_label) ||
		(rotation_meadow == land_use_label) ||
		(close_seeded_legumes == land_use_label))
			return(0.29);

	else if ((pasture == land_use_label)
		  || (range == land_use_label) && 
	          (strcmp("poor",hy_cond_label) == 0))
			return(0.01);

	else if ((pasture == land_use_label) 
		  || (range == land_use_label) && 
		  (strcmp("fair",hy_cond_label) == 0))
			return(0.15);

	else if (((pasture == land_use_label)
		  || (range == land_use_label)) && 
		  (strcmp("good",hy_cond_label) == 0))
			return(0.22);

	else if ((meadow == land_use_label) || 
		(permanent_meadow == land_use_label))
			return(0.59);

	else if ((woods == land_use_label) || 
		 (woodland == land_use_label)) 
			return(0.29);

	else if (grass_waterway == land_use_label)
			return(1.0);

	else if ((hard_surface == land_use_label) ||
		 (farmsteads == land_use_label) ||
		 (roads_(dirt) == land_use_label) ||
		 (urban == land_use_label))
			return(0.01);

	else if ((water == land_use_label) ||
		 (marsh == land_use_label))
			return(0.0);

	else
	{
	   fprintf (stderr,"The combination of %s, %s and %s is not defined!!\n Please reclassify the map and refer AGNPS user manaual for proper surface condition constant\n",land_use_label,mgmt_pr,hy_cond_label);
	   clean_up();
	   exit(0);
	}
}

