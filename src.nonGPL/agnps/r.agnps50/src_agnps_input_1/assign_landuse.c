
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
	
	int assign_landuse(land_use_label)

	To assign the landuse values based on the landuse
	and returns the corresponding landuse value. 
	This saves lot of time spend otherwise one need to do
	strcmp for each routines that access the landuse label info.
*/

#include "agnps_input.h"


int assign_landuse(land_use_label)
char	*land_use_label;

{


	if (strcmp("fallow",land_use_label) == 0)  return(fallow);

	else if ((strcmp("row crops",land_use_label) == 0) ||
	         (strcmp("row crop",land_use_label) == 0)) return(row_crops);

	else if (strcmp("small grain",land_use_label) == 0) return(small_grain);

	else if(strcmp("legumes",land_use_label) == 0) return(legumes);

	else if (strcmp("rotation meadow",land_use_label) == 0) return(rotation_meadow);
	else if(strcmp("close-seeded legumes",land_use_label) == 0) return(close_seeded_legumes);

	else if (strcmp("pasture",land_use_label) == 0) return(pasture);
	else if(strcmp("range",land_use_label) == 0) return (range);

	else if (strcmp("meadow",land_use_label) == 0) return(meadow);
	else if(strcmp("permanent meadow",land_use_label) == 0) return(permanent_meadow);

	else if (strcmp("woods",land_use_label) == 0) return(woods);
	else if(strcmp("woodland",land_use_label) == 0) return(woodland);

	else if (strcmp("grass waterway",land_use_label) == 0) return(grass_waterway);

	else if (strcmp("hard surface",land_use_label) == 0) return(hard_surface);
	else if(strcmp("farmsteads",land_use_label) == 0) return(farmsteads);
	else if(strcmp("farmstead",land_use_label) == 0) return(farmsteads);
	else if(strcmp("roads (dirt)",land_use_label) == 0) return(roads_(dirt));
	else if(strcmp("urban",land_use_label) == 0) return(urban);

	else if (strcmp("water",land_use_label) == 0) return(water);
	else if (strcmp("marsh",land_use_label) == 0) return(marsh);

	else
	{
	   fprintf (stderr,"The %s type landuse is not defined!!\n Please reclassify the map \n",land_use_label);
	   clean_up();
	   exit(0);
	}
}
