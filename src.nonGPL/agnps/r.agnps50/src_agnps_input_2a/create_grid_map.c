



      /*  Function  C R E A T E_G R I D_M A P        */
      /*                                             */
      /*  This function is used to create grid map   */
      /*  for any resolution given by the user.  This*/
      /*  grid map is used to create flow map from   */
      /*  the base elevation map.                    */
      /*                                             */


      /*               Zhian Li, 8/8/95              */
      /*                                             */
      /*               Penn State University         */ 
        

 
#include "agnps_input.h"


      create_grid_map()
      
        {

	int	i, j, lct;
	int     north_row, south_row, east_col, west_col;
	int	nrow, ncol;
	FILE	*fopen();
	char	buf[128];
	double  north, south, east, west;
        char    buf1[100];

       /* Define a work map                         */
        MAPS    *work_map;


       /* get the original window information from  */
       /* the watershed cell header file.           */
	fprintf (stderr,"Creating grid map for watershed %s\n",wshd->p);

	G_get_cellhd(wshd->p,wshd->mapset,&orig_window);

       /* Reset the window and set all the window   */ 
       /* parameters so as to get correct grid map  */

	fprintf (stderr,"The grid size selected by the user is %d meters \n",grid_res);

	if(grid_res != 0){
	   orig_window.ns_res = (double) grid_res;
	   orig_window.ew_res = (double) grid_res;
	   orig_window.rows = 
             (int)((orig_window.north - orig_window.south)/ orig_window.ns_res);
	   orig_window.cols= 
             (int)((orig_window.east - orig_window.west)/ orig_window.ew_res);
	   orig_window.south = 
             orig_window.north - (orig_window.rows* orig_window.ns_res);
	   orig_window.west = 
             orig_window.east - (orig_window.cols* orig_window.ew_res);
	   }

        fprintf (stderr,"After window reset\n");
	G_set_window(&orig_window);
	G_put_window(&orig_window);
        fprintf (stderr,"After put window in effect \n");

	nrow = orig_window.rows;
	ncol = orig_window.cols;


	wshd->fd = cell_open(wshd->p,wshd->mapset);
        fprintf (stderr,"After water shed map \n");

	strcpy(temp_grid_map->p, "temp_grid_map");
        fprintf (stderr,"After copy \n");
	temp_grid_map->fd = cell_open_new(temp_grid_map->p);
        fprintf (stderr,"After open temp grid map \n");
	temp_grid_map->rbuf = G_allocate_cell_buf();
        fprintf (stderr,"After allocate memory \n");



     /* Make a copy of the elevation map for masking*/

        sprintf(buf1,"g.copy rast=%s.elev,work_elev.map\n",wshd_name);
        system(buf1);


     /* Allocate memory for temp_grid_map       */

        work_map = (MAPS *) emalloc ((unsigned) sizeof (MAPS));
        work_map->p = emalloc ((unsigned) (64));


        strcpy(work_map->p,"work_elev.map");
        work_map->mapset = get_mapset(work_map->p);
        work_map->fd = cell_open(work_map->p,work_map->mapset);
        work_map->rbuf = G_allocate_cell_buf();

	lct = 1;
		 
	for(i = 0; i < nrow; i++) {
	   G_get_map_row(work_map->fd,work_map->rbuf,i);
	   G_zero_cell_buf(temp_grid_map->rbuf);

	   for(j=0;j < ncol;j++){
              if(work_map->rbuf[j] > 0 ) {
                 temp_grid_map->rbuf[j] = lct;
                 lct++;
                 }
	      }

	   G_put_map_row(temp_grid_map->fd,temp_grid_map->rbuf);
	   }


	G_close_cell(temp_grid_map->fd);
	G_close_cell(work_map->fd);


        }
