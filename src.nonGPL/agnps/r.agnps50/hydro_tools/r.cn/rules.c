/*	January, 1991	Agricultural Engineering, Purdue University
	Raghavan Srinivasan (srin@ecn.purdue.edu)
	
	int rules(veg_cover,land_use,hy_cond)

	To assign the row number from the curve number table based on
	the veg_cover, land_use and hy_cond labels in the corresponding
	maps and returns the row number. The referred CN table used
	was from SWABB manual.
*/

#include "CN.h"


int rules(veg_cover,land_use,hy_cond)
CELL	veg_cover,land_use, hy_cond;

{
	char	*veg_cover_label, *land_use_label, *hy_cond_label;


	veg_cover_label = G_get_cat(veg_cover, &veg_cover_cats);
	land_use_label = G_get_cat(land_use, &land_use_cats);
	hy_cond_label = G_get_cat(hy_cond, &hy_cond_cats);

	if ((strcmp("fallow",land_use_label) == 0) && 
	    (strcmp("straight row",veg_cover_label) == 0))
			return(0);

	else if ((strcmp("row crops",land_use_label) == 0) && 
		 ((strcmp("straight row",veg_cover_label) == 0) &&
		  (strcmp("poor",hy_cond_label) == 0)))
			return(1);

	else if ((strcmp("row crops",land_use_label) == 0) && 
		 ((strcmp("straight row",veg_cover_label) == 0) &&
		  (strcmp("good",hy_cond_label) == 0)))
			return(2);

	else if ((strcmp("row crops",land_use_label) == 0) && 
		 ((strcmp("contoured",veg_cover_label) == 0) &&
		  (strcmp("poor",hy_cond_label) == 0)))
			return(3);

	else if ((strcmp("row crops",land_use_label) == 0) && 
		 ((strcmp("contoured",veg_cover_label) == 0) &&
		  (strcmp("good",hy_cond_label) == 0)))
			return(4);

	else if ((strcmp("row crops",land_use_label) == 0) && 
		 ((strcmp("contoured and terraced",veg_cover_label) == 0) &&
		  (strcmp("poor",hy_cond_label) == 0)))
			return(5);

	else if ((strcmp("row crops",land_use_label) == 0) && 
		 ((strcmp("contoured and terraced",veg_cover_label) == 0) &&
		  (strcmp("good",hy_cond_label) == 0)))
			return(6);

	else if ((strcmp("small grain",land_use_label) == 0) && 
		 ((strcmp("straight row",veg_cover_label) == 0) &&
		  (strcmp("poor",hy_cond_label) == 0)))
			return(7);

	else if ((strcmp("small grain",land_use_label) == 0) && 
		 ((strcmp("straight row",veg_cover_label) == 0) &&
		  (strcmp("good",hy_cond_label) == 0)))
			return(8);

	else if ((strcmp("small grain",land_use_label) == 0) && 
		 ((strcmp("contoured",veg_cover_label) == 0) &&
		  (strcmp("poor",hy_cond_label) == 0)))
			return(9);

	else if ((strcmp("small grain",land_use_label) == 0) && 
		 ((strcmp("contoured",veg_cover_label) == 0) &&
		  (strcmp("good",hy_cond_label) == 0)))
			return(10);

	else if ((strcmp("small grain",land_use_label) == 0) && 
		 ((strcmp("contoured and terraced",veg_cover_label) == 0) &&
		  (strcmp("poor",hy_cond_label) == 0)))
			return(11);

	else if ((strcmp("small grain",land_use_label) == 0) && 
		 ((strcmp("contoured and terraced",veg_cover_label) == 0) &&
		  (strcmp("good",hy_cond_label) == 0)))
			return(12);

	else if (((strcmp("close-seeded legumes",land_use_label) == 0)
		  || (strcmp("rotation meadow",land_use_label) == 0)) && 
		 ((strcmp("straight row",veg_cover_label) == 0) &&
		  (strcmp("poor",hy_cond_label) == 0)))
			return(13);

	else if (((strcmp("close-seeded legumes",land_use_label) == 0)
		  || (strcmp("rotation meadow",land_use_label) == 0)) && 
	    ((strcmp("straight row",veg_cover_label) == 0) &&
	     (strcmp("good",hy_cond_label) == 0)))
			return(14);

	else if (((strcmp("close-seeded legumes",land_use_label) == 0)
		  || (strcmp("rotation meadow",land_use_label) == 0)) && 
	    ((strcmp("contoured",veg_cover_label) == 0) &&
	     (strcmp("poor",hy_cond_label) == 0)))
			return(15);

	else if (((strcmp("close-seeded legumes",land_use_label) == 0)
		  || (strcmp("rotation meadow",land_use_label) == 0)) && 
	    ((strcmp("contoured",veg_cover_label) == 0) &&
	     (strcmp("good",hy_cond_label) == 0)))
			return(16);

	else if (((strcmp("close-seeded legumes",land_use_label) == 0)
		  || (strcmp("rotation meadow",land_use_label) == 0)) && 
	    ((strcmp("contoured and terraced",veg_cover_label) == 0) &&
	     (strcmp("poor",hy_cond_label) == 0)))
			return(17);

	else if (((strcmp("close-seeded legumes",land_use_label) == 0)
		  || (strcmp("rotation meadow",land_use_label) == 0)) && 
	    ((strcmp("contoured and terraced",veg_cover_label) == 0) &&
	     (strcmp("good",hy_cond_label) == 0)))
			return(18);

	else if (((strcmp("pasture",land_use_label) == 0)
		  || (strcmp("range",land_use_label) == 0)) && 
	    ((strcmp("contoured",veg_cover_label) != 0) &&
	     (strcmp("poor",hy_cond_label) == 0)))
			return(19);

	else if (((strcmp("pasture",land_use_label) == 0)
		  || (strcmp("range",land_use_label) == 0)) && 
		 ((strcmp("contoured",veg_cover_label) != 0) &&
		  (strcmp("fair",hy_cond_label) == 0)))
			return(20);

	else if (((strcmp("pasture",land_use_label) == 0)
		  || (strcmp("range",land_use_label) == 0)) && 
		 ((strcmp("contoured",veg_cover_label) != 0) &&
		  (strcmp("good",hy_cond_label) == 0)))
			return(21);

	else if (((strcmp("pasture",land_use_label) == 0)
		  || (strcmp("range",land_use_label) == 0)) && 
		 ((strcmp("contoured",veg_cover_label) == 0) &&
		  (strcmp("poor",hy_cond_label) == 0)))
			return(22);

	else if (((strcmp("pasture",land_use_label) == 0)
		  || (strcmp("range",land_use_label) == 0)) && 
		 ((strcmp("contoured",veg_cover_label) == 0) &&
		  (strcmp("fair",hy_cond_label) == 0)))
			return(23);

	else if (((strcmp("pasture",land_use_label) == 0)
		  || (strcmp("range",land_use_label) == 0)) && 
		 ((strcmp("contoured",veg_cover_label) == 0) &&
		  (strcmp("good",hy_cond_label) == 0)))
			return(24);

	else if ((strcmp("meadow",land_use_label) == 0) && 
		 (strcmp("good",hy_cond_label) == 0))
			return(25);

	else if ((strcmp("woods",land_use_label) == 0) && 
		 (strcmp("poor",hy_cond_label) == 0))
			return(26);

	else if ((strcmp("woods",land_use_label) == 0) && 
		 (strcmp("fair",hy_cond_label) == 0))
			return(27);

	else if ((strcmp("woods",land_use_label) == 0) && 
		 (strcmp("good",hy_cond_label) == 0))
			return(28);

	else if (strcmp("farmsteads",land_use_label) == 0)
			return(29);

	else if (strcmp("roads (dirt)",land_use_label) == 0)
			return(30);

	else if (strcmp("hard surface",land_use_label) == 0)
			return(31);

	else
	{
	   fprintf (stderr,"The combination of %s, %s and %s is not defined!!\n Please reclassify the map and refer USDA Curver Number table\n",land_use_label,veg_cover_label,hy_cond_label);
	   exit(0);
	}
}

