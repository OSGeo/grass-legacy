
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
	
	float rules_ch_side_slope(clay, silt, sand, type_of_ch)

	To assign the channel side slope, it is assumed to be
	a open type channel. A value equivalent to half of the side
	slope suggested by "Soil ans water conservation engineering"
	book by Schwab (pp 293) is used depending on the texture of
	soil. If the type of channgel is specified like open channel
	or grassed waterways then channel slope according to the
	type of channel is used. The default type of channel is
	open channel. 
*/

/* why not use the soil texture rules here rather than passing clay, silt
   and sand values, check the soil texture rules once again */

#include "agnps_input.h"


float rules_ch_side_slope(clay, silt, sand, type_of_ch)
int	type_of_ch;
int	clay, sand, silt;

{

	if(grass_waterway == type_of_ch){
	   if(clay >= 50 && sand <= 50 && silt <= 50) return(17.0); /* 6:1 */
	   else if(clay >= 50 && sand <= 50 && silt > 50) return(12.0); /* 8:1 */
	   else if(clay <= 50 && sand >= 50 && clay >= 20) return(10.0); /* 10:1*/
	   else if(sand >= 50 && clay <= 20) return(8.0); /* 12:1 */
	   }
	else{
	   if(clay >= 50 && sand <= 50 && silt <= 50) return(100.0); /* 0.51:1 */
	   else if(clay >= 50 && sand <= 50 && silt > 50) return(50.0); /* 1:1 */
	   else if(clay <= 50 && sand >= 50 && clay >= 20) return(33.3); /*1.5:1*/
	   else if(sand >= 50 && clay <= 20) return(12.5); /* 2:1 */
	   }

	return(10.0); /* default from AGNPS users guide */

	/*
	   fprintf (stderr,"The combination of %d, %d and %d is not defined!!\n Please reclassify the map and refer AGNPS user manaual for proper channel side slope \n",clay,sand,silt);
	   clean_up();
	   exit(0);
	*/
}
