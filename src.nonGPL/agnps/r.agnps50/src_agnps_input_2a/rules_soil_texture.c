
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
	
	int rules_soil_texture(clay, silt, sand)

	To assign the texture of a soil. The soil texture triangle
	shown in page 19 of AGNPS  research report 35 of USDA, ARS
	manual. The clay, silt, and sand values are derived from the
	soils map and Soils-5 database.
*/

#include <stdio.h>
#define WATER     0
#define SAND      1
#define SILT      2
#define CLAY      3
#define PEAT      4


int rules_soil_texture(clay, silt, sand)
int	clay, sand, silt;

{

	   if(clay >= 40 && sand <= 20 && silt <= 60) return(CLAY);
	   else if(clay >= 30 && sand <= 45 && silt <= 52) return(CLAY);
	   else if(clay <= 60 && sand >= 45 && silt <= 25) return(SAND);
	   else if(clay <= 60 && sand >= 60 && silt <= 50) return(SAND);
	   else return(SILT);

	   /*
	   fprintf (stderr,"The combination of %d, %d and %d is not defined!!\n Please reclassify the map and refer AGNPS user manaual for proper soil texture \n",clay,sand,silt);
	   clean_up();
	   exit(0);
	   */
}
