
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

/*	June, 1991  Agricultural Engineering, Purdue University
	Raghavan Srinivasan (srin@ecn.purdue.edu)
	
	get_input_map_names()

	To get all the 8 input layers of informations required to
	prepare the agnps input file.
*/


       /*  Modified by Zhian Li at Penn State University */
       /*  so that the code will create the aspect map   */
       /*  from the elevation map if the requested aspect*/
       /*  does not exist.                               */
       

#include "agnps_input.h"


get_input_map_names()
{
	char buf[512], *buf1, buf2[512];

        char buf3[1024],buf4[512];

        char buf5[5];

	map_mem_alloc();



/* get the watershed outline map for masking */
/* concatenate the wshd_name with . and proper extension */

	sprintf(buf2,"%s.wshd",wshd_name);
	if((find_old_map(buf2)) == 1) strcpy(wshd->p,buf2);
	else { 
		buf1 = get_old_name("Watershed Map for Masking", 1);

		strcpy(wshd->p,buf2);
		}
	wshd->mapset = get_mapset(wshd->p);
	wshd->rbuf = G_allocate_cell_buf();

/* get the elevation map to find slope */
	sprintf(buf2,"%s.elev",wshd_name);
	sprintf(buf4,"%s.elev",wshd_name);
	if((find_old_map(buf2)) == 1) strcpy(elev->p,buf2);
	else { 
		buf1 = get_old_name("Elevation Map", 1);
		strcpy(elev->p,buf1);
		}
	elev->mapset = get_mapset(elev->p);
	elev->fd = cell_open(elev->p, elev->mapset);
	elev->rbuf = G_allocate_cell_buf();

/* get the aspect map */
	sprintf(buf2,"%s.asp.%d",wshd_name,grid_res);
	if((find_old_map(buf2)) == 1) strcpy(temp_dir_map->p,buf2);
	else { 
              /*
		buf1 = get_old_name("Aspect Map", 1);
               */
     /*  Create an aspect map from the elevation map     */
     
     
                create_grid_map();
    
                sprintf(buf3,"$GISBASE/etc/agnps50/create_asp_map %s %s\n",buf2,buf4);
  fprintf (stderr,"The content fo buf3 is: %s\n",buf3);
  fprintf (stderr,"Creating temporary work map\n");
  fprintf (stderr,"Please wait.  It may take a few minutes\n");
                system(buf3);
                system("clear");
                fprintf (stderr,"  \n");
                fprintf (stderr,"  \n");
                fprintf (stderr,"A NEW FLOW MAP %s  HAS BEEN CREATED\n",buf2);
                fprintf (stderr,"  \n");
                fprintf (stderr,"HOWEVER, THE FLOW DIRECTIONS ARE NOT GUARANTEED TO\n");
                fprintf (stderr,"  \n");
                fprintf (stderr,"BE CORRECT FOR ALL CELLS BECAUSE OF THE INHERENT\n");
                fprintf (stderr,"  \n");
                fprintf (stderr,"LIMITATION OF GRASS FUNCTIONS.\n");
                fprintf (stderr,"  \n");
                fprintf (stderr,"THE USER IS URGED TO EDIT THIS MAP FIRST!!!\n");
                fprintf (stderr,"  \n");
                fprintf (stderr,"  \n");
                fprintf (stderr,"Please edit the flow map and run again \n");
                fprintf (stderr,"  \n");
                fprintf (stderr,"  \n");
                /*
                gets(buf5);
                if((strncmp(buf5,"Y",1)) == 0) exit(0);
                if((strncmp(buf5,"y",1)) == 0) exit(0);
		strcpy(temp_dir_map->p,buf2);
                */
                exit(0);
		}
	temp_dir_map->mapset = get_mapset(temp_dir_map->p);
	temp_dir_map->fd = cell_open(temp_dir_map->p, temp_dir_map->mapset);
	temp_dir_map->rbuf = G_allocate_cell_buf();

/* check for soil attributes map first, if not then get soils map */

/* check for K factor map */
	sprintf(buf2,"%s.K",wshd_name);
	if((find_old_map(buf2)) == 1) strcpy(K_fac->p,buf2);
	else { 
		do{
		fprintf (stderr,"Do you have a USLE K factor map (y/n) ");
		} while(!G_gets(buf));
		   if(buf[0] == 'y' || buf[0] == 'Y'){
		   buf1 = get_old_name("USLE K factor map", 1);
		   strcpy(K_fac->p,buf1);
		   }
		}
	K_fac->mapset = get_mapset(K_fac->p);
	K_fac->fd = cell_open(K_fac->p, K_fac->mapset);
	K_fac->rbuf = G_allocate_cell_buf();

			strcpy(buf,"");
/* check for hydrological soil group map */
	sprintf(buf2,"%s.hyg",wshd_name);
	if((find_old_map(buf2)) == 1) strcpy(hyg->p,buf2);
	else { 
		do{
		fprintf (stderr,"Do you have a hydrological soil group map (y/n) ");
		} while(!G_gets(buf));
		   if(buf[0] == 'y' || buf[0] == 'Y'){
		   buf1 = get_old_name("Hydrological soil group map", 1);
		   strcpy(hyg->p,buf1);
		   }
		}
	hyg->mapset = get_mapset(hyg->p);
	hyg->fd = cell_open(hyg->p, hyg->mapset);
	hyg->rbuf = G_allocate_cell_buf();

			strcpy(buf,"");
/* check for sand map */
	sprintf(buf2,"%s.sand",wshd_name);
	if((find_old_map(buf2)) == 1) strcpy(sand->p,buf2);
	else { 
		do{
		fprintf (stderr,"Do you have a sand texture map (y/n) ");
		} while(!G_gets(buf));
		   if(buf[0] == 'y' || buf[0] == 'Y'){
		   buf1 = get_old_name("Sand texture Map", 1);
		   strcpy(sand->p,buf1);
		   }
		}
	sand->mapset = get_mapset(sand->p);
	sand->fd = cell_open(sand->p, sand->mapset);
	sand->rbuf = G_allocate_cell_buf();

			strcpy(buf,"");
/* check for clay map */
	sprintf(buf2,"%s.clay",wshd_name);
	if((find_old_map(buf2)) == 1) strcpy(clay->p,buf2);
	else { 
		do{
		fprintf (stderr,"Do you have a clay texture map (y/n) ");
		} while(!G_gets(buf));
		   if(buf[0] == 'y' || buf[0] == 'Y'){
		   buf1 = get_old_name("Clay texture Map", 1);
		   strcpy(clay->p,buf1);
		   }
		}
	clay->mapset = get_mapset(clay->p);
	clay->fd = cell_open(clay->p, clay->mapset);
	clay->rbuf = G_allocate_cell_buf();
			strcpy(buf,"");


/* get the land use map to find the CN and C values */
	sprintf(buf2,"%s.luse",wshd_name);
	if((find_old_map(buf2)) == 1) strcpy(landuse->p,buf2);
	else { 
		buf1 = get_old_name("Landuse Map", 1);
		strcpy(landuse->p,buf1);
		}
	landuse->mapset = get_mapset(landuse->p);
	landuse->fd = cell_open(landuse->p, landuse->mapset);
	landuse->rbuf = G_allocate_cell_buf();

/* get the management practice map to find the CN and C values */
	sprintf(buf2,"%s.mgpr",wshd_name);
	if((find_old_map(buf2)) == 1) strcpy(mgt_practice->p,buf2);
	else { 
		buf1 = get_old_name("Management Practice Map", 1);
		strcpy(mgt_practice->p,buf1);
		}
	mgt_practice->mapset = get_mapset(mgt_practice->p);
	mgt_practice->fd = cell_open(mgt_practice->p, mgt_practice->mapset);
	mgt_practice->rbuf = G_allocate_cell_buf();

/* get the nutient maps to find the fertilizer factor */
	sprintf(buf2,"%s.nut",wshd_name);
	if((find_old_map(buf2)) == 1) strcpy(nutrient->p,buf2);
	else { 
		buf1 = get_old_name("Nutrients Map", 1);
		strcpy(nutrient->p,buf1);
		}
	nutrient->mapset = get_mapset(nutrient->p);
	nutrient->fd = cell_open(nutrient->p, nutrient->mapset);
	nutrient->rbuf = G_allocate_cell_buf();

/***************************************************************/
/* start of code section added by Dave Peterson, February 1996 */
/***************************************************************/

/* get the pesticide maps */
        sprintf(buf2, "%s.pest", wshd_name);
	if ((find_old_map(buf2)) == 1) strcpy(pesticide -> p, buf2);
	else
	 { buf1 = get_old_name("Pesticides Map", 0);
	   if (buf1 != NULL) strcpy(pesticide -> p, buf1);
	   else              pesticide -> p = NULL;
	 }

        if (pesticide -> p == NULL)
	 { pesticide -> mapset = NULL;
	   pesticide -> rbuf = NULL;
	 }
	else
	 { pesticide -> mapset = get_mapset(pesticide -> p);
           pesticide -> fd     = cell_open(pesticide -> p, pesticide -> mapset);
           pesticide -> rbuf   = G_allocate_cell_buf();
         }

/*************************************************************/
/* end of code section added by Dave Peterson, February 1996 */
/*************************************************************/

/* get the implements used map to find the fertilizer availability factor */
	sprintf(buf2,"%s.mach",wshd_name);
	if((find_old_map(buf2)) == 1) strcpy(machinery->p,buf2);
	else { 
		buf1 = get_old_name("Machinery Map", 1);
		strcpy(machinery->p,buf1);
		}

/* get the channel slope map to route */
/*
	sprintf(buf2,"%s.chsl",wshd_name);
	if((find_old_map(buf2)) == 1){
		strcpy(channel_slope->p,buf2);
		channel_slope->mapset = get_mapset(channel_slope->p);
		channel_slope->fd = cell_open(channel_slope->p, channel_slope->mapset);
		channel_slope->rbuf = G_allocate_cell_buf();
		channel_slope->flag = YES;
		}
	else { 
*/ 
/*  The following lines of code were commented out by Zhian Li */
/*  because normally user will not have a cahnnel slope map    */

/*
		do{
		fprintf (stderr,"Do you have a Channel Slope map (y/n)\n ");
		fprintf (stderr,"If no, 50%% of overland slope is assumed as recommendad by the AGNPS manual\n "); 

		} while(!G_gets(buf));
 
*/
/*
                   strcpy(buf,"no"); 

		   if(buf[0] == 'y' || buf[0] == 'Y'){
		      buf1 = get_old_name("Channel Slope Map", 1);
		      strcpy(channel_slope->p,buf1);
		      channel_slope->mapset = get_mapset(channel_slope->p);
		      channel_slope->fd = cell_open(channel_slope->p, channel_slope->mapset);
		      channel_slope->rbuf = G_allocate_cell_buf();
		      channel_slope->flag = YES;
		      }
		   else channel_slope->flag = NO;
		  } 
*/

/* get the C factor map */
	sprintf(buf2,"%s.C",wshd_name);
	if((find_old_map(buf2)) == 1) strcpy(C_fac->p,buf2);
	else { 
		buf1 = get_old_name("USLE C Factor Map", 1);
		strcpy(C_fac->p,buf1);
		}
}

map_mem_alloc()
{
	wshd = (MAPS *) emalloc ((unsigned) sizeof (MAPS));
	wshd->p = emalloc ((unsigned) (64));

	elev = (MAPS *) emalloc ((unsigned) sizeof (MAPS));
	elev->p = emalloc ((unsigned) (64));

	soils = (MAPS *) emalloc ((unsigned) sizeof (MAPS));
	soils->p = emalloc ((unsigned) (64));

	landuse = (MAPS *) emalloc ((unsigned) sizeof (MAPS));
	landuse->p = emalloc ((unsigned) (64));

	mgt_practice = (MAPS *) emalloc ((unsigned) sizeof (MAPS));
	mgt_practice->p = emalloc ((unsigned) (64));

	nutrient = (MAPS *) emalloc ((unsigned) sizeof (MAPS));
	nutrient->p = emalloc ((unsigned) (64));

/***************************************************************/
/* start of code section added by Dave Peterson, February 1996 */
/***************************************************************/

        pesticide = (MAPS *) emalloc ((unsigned) sizeof(MAPS));
	pesticide -> p = emalloc((unsigned) (64));

/*************************************************************/
/* end of code section added by Dave Peterson, February 1996 */
/*************************************************************/


	machinery = (MAPS *) emalloc ((unsigned) sizeof (MAPS));
	machinery->p = emalloc ((unsigned) (64));

	channel_slope = (MAPS *) emalloc ((unsigned) sizeof (MAPS));
	channel_slope->p = emalloc ((unsigned) (64));


/* allocate memory for the newly created maps */

	cell_num_map = (MAPS *) emalloc ((unsigned) sizeof (MAPS));
	cell_num_map->p = emalloc ((unsigned) (64));

	temp_slope_map = (MAPS *) emalloc ((unsigned) sizeof (MAPS));
	temp_slope_map->p = emalloc ((unsigned) (64));

	temp_dir_map = (MAPS *) emalloc ((unsigned) sizeof (MAPS));
	temp_dir_map->p = emalloc ((unsigned) (64));

	C_fac = (MAPS *) emalloc ((unsigned) sizeof (MAPS));
	C_fac->p = emalloc ((unsigned) (64));

	hyg = (MAPS *) emalloc ((unsigned) sizeof (MAPS));
	hyg->p = emalloc ((unsigned) (64));

	K_fac = (MAPS *) emalloc ((unsigned) sizeof (MAPS));
	K_fac->p = emalloc ((unsigned) (64));

	sand = (MAPS *) emalloc ((unsigned) sizeof (MAPS));
	sand->p = emalloc ((unsigned) (64));

	clay = (MAPS *) emalloc ((unsigned) sizeof (MAPS));
	clay->p = emalloc ((unsigned) (64));

	hy_cond = (MAPS *) emalloc ((unsigned) sizeof (MAPS));
	hy_cond->p = emalloc ((unsigned) (64));

	temp_cn_map = (MAPS *) emalloc ((unsigned) sizeof (MAPS));
	temp_cn_map->p = emalloc ((unsigned) (64));

     /* Allocate memory for temp_grid_map       */

	temp_grid_map = (MAPS *) emalloc ((unsigned) sizeof (MAPS));
	temp_grid_map->p = emalloc ((unsigned) (64));


}

char *emalloc (n)
unsigned n;
{
    char *p, *malloc ();

    if ((p = malloc (n)) == NULL) {
	clean_up();
    	exit (1);
	}
    return (p);
}

