
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
	
	int rules_fert_aval(implements, land_use_label)

	To assign the fertilizer availabilit factor that would be
	available at top 1 cm of soil. This is done based on the
	type of implements used (user supplied map), and if none exists
	in that location and if the land use is urban or water the
	values are chosen appropriatly. The table used for this
	is from the AGNPS manual (page 19).
*/

#include "agnps_input.h"


int rules_fert_aval(implements, land_use_label)
char	*implements;
int	land_use_label;

{


	if (strcmp("large offset disk",implements) == 0)  return(40);

	else if (strcmp("moldboard plow",implements) == 0)  return(10);
	else if (strcmp("lister",implements) == 0)  return(20);
	else if (strcmp("chisel plow",implements) == 0)  return(67);
	else if (strcmp("disk",implements) == 0)  return(50);
	else if (strcmp("field cultivator",implements) == 0)  return(70);
	else if (strcmp("row cultivator",implements) == 0)  return(50);
	else if (strcmp("anhydrous applicator",implements) == 0)  return(85);
	else if (strcmp("rod weeder",implements) == 0)  return(95);
	else if (strcmp("planter",implements) == 0)  return(85);
	else if ((strcmp("smooth",implements) == 0) ||
		 (strcmp("no till", implements) == 0))
		return(100);

	else if ((hard_surface == land_use_label) ||
		 (farmsteads == land_use_label) ||
		 (roads_(dirt) == land_use_label) ||
		 (urban == land_use_label))
			return(100);

	else if ((water == land_use_label) ||
		 (marsh == land_use_label))
			return(0);

	else
	{
	   fprintf (stderr,"The combination of %s or %d is not defined!!\n Please reclassify the map and refer AGNPS user manaual for proper fertilizer availability factor \n",implements, land_use_label);
	   clean_up();
	   exit(0);
	}
}

