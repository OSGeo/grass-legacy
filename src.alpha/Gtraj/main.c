/****************************************************************/
/*								*/
/*	main.c		in	~/src/Gtraj			*/
/*								*/
/*	This is the main program for the trajectory analysis	*/
/*								*/
/****************************************************************/

#include "segment.h"
#include "gis.h"
#define MAIN
#include "cmd_line.h"
#include "point.h"
#include "radians.h"
#include "constants.h"
#include <stdio.h>

#define         COLOR_SHIFT      500.0
#define         COLOR_MAX        900.0

#define USAGE   " elev_layer=name  east=value  north=value  weapon=name  ammunition=name  weapon_elev=value  azimuth1=value  azimuth2=value  out_layer=name"
 
#define		NEXT_SEARCH_PT		SEARCH_PT->next
#define		SEARCH_PT_HIGH_ANGLE	SEARCH_PT->high_angle

CELL 	firept_elev,	/* absolute elevation of the gun   	*/
	pt_elev;  
 
char buf[1024];
struct Cell_head window;
 
int row_firept,col_firept;     /* map array coors of firing pt.	*/
int nrows,ncols;
 
double tent_max_dist;   /* tentative max range of fall  	*/

double range1, range2;	/* radian angles defining fire fan	*/
double low_angle, 	/* low angle of fire of weapon		*/
	high_angle, 	/* high angle of fire of weapon		*/
	vel_initial;	/* muzzle velocity of weapon		*/
double DIAMETER, MASS;	/* properties of the shell		*/
double TOL_X;
 
int total;      	/* total pts picked up in first list 	*/
 
 
main(argc,argv)
     int argc;
     char *argv[];
{
	FILE *fpr, *fopen();
	char weapons_file[200], ammunition_file[200];
        char *current_mapset,
        *search_mapset,
        *old_mapset; /* pnts to the mapset of the elev_layer 	*/

        CELL *cell,		/* pointer to CELL i/o buffer	*/
              data;


  /* structure to hold the header info of the elevation layer */
         struct Cell_head cellhd;

    int srows,scols,        /* rows and cols in every segment */
        len;                /* size of a cell entry (bytes)   */
        int stash_away(),set_default_options();
								
        int new                 /* fd for output overlay        */
        ,old,                   /* fd for elevation overlay     */
        in_fd,                  /* fd for segmented elev layer  */
        out_fd;                 /* fd for segmented output layer*/

        int row;                /* row counter used in looping  */

       	SEGMENT seg_in, seg_out;

        struct Colors colors;
	struct Range range;
        char *value;
 
        int i,j;        /* looping variables.(0,0) at left top  */
 
        POINT 	*make_list(),
		*sort_list(),
		*unhittable_pts_elimination(),
                *head, *c_ptr, *present_point, *SEARCH_PT;
 
        double max_vert_angle = 0.0, factor;
        double decide_color_range(), azimuth_to_angle(), fabs();
 
        /* check if the user has provided the correct command   */
        /* line options                                         */
        if(argc < 10)
        {
        fprintf(stderr,"Usage: %s %s\n", argv[0], USAGE) ;
        exit(-1) ;
        }

        /* gis database initialization  */
        G_gisinit (argv[0]);

        /* name of current mapset       */
        current_mapset= G_mapset();

        set_default_options();

	if(D_parse_command(argc,argv,variables,n_variables, 
						stash_away))
        {
        fprintf(stderr,"Usage: %s %s\n", argv[0], USAGE) ;
        exit(-1) ;
        }

	/* determine range of fire				*/
	range1 = azimuth_to_angle(azimuth1);
	range2 = azimuth_to_angle(azimuth2);

        /* read in database window parameters   */
        if (G_get_window (&window) < 0)
    	{   
	sprintf (buf, "can't read database window parameters");
        G_fatal_error (buf);
        exit(1);
    	}
 
   	/* check if weapon location is inside database window   */
        if(east <window.west || east >window.east 
		|| north >window.north || north <window.south)
        {
	sprintf (buf,"weapon location outside database window");
        G_fatal_error (buf);
        exit(1);
    	}
 
        /* check if the elevation overlay exists        	*/
        search_mapset = "";
 
        old_mapset = G_find_cell (elev_layer, search_mapset);
    	if (old_mapset == NULL)
    	{
        sprintf (buf, "%s - not found", elev_layer);
        G_fatal_error (buf);
        exit(1);
    	}
 
        /* check if the output layer has a legal filename       */
        if (G_legal_filename(out_layer) < 0)
    	{
        sprintf (buf, "%s - illegal name", out_layer);
        G_fatal_error (buf);
        exit(1);
   	}
 
        /* check if the output layer already exists in the      */
        /* user's current mapset                                */
        if (G_find_cell (out_layer,current_mapset))
    	{
 	sprintf (buf, "%s - already exits. can't overwrite", 
						out_layer);
        G_fatal_error (buf);
        exit(1);
    	}
 
        /* read the cell header into the appropriate structure  */
        if (G_get_cellhd (elev_layer, old_mapset, &cellhd) < 0)
    	{
	sprintf(buf,"%s in %s - can't read cell header",
					elev_layer,old_mapset);
        G_fatal_error (buf);
        exit(1);
    	}

	/* read weapon and ammunition data from weapon database	*/
	sprintf(weapons_file, "%s/weapon_data/weapons",	
						G_gisbase());
	sprintf(ammunition_file, "%s/weapon_data/ammunition",
						G_gisbase());

	fpr = fopen(weapons_file, "r");
	if(fpr == NULL)
	{
	G_fatal_error("can't open weapons file");
	}
		
	while (1)
	{
	if(fscanf(fpr, "%10s%10lf%10lf", 
			buf, &high_angle, &low_angle) == EOF)
		{
	sprintf(buf, "Weapon not listed in database") ;
	G_fatal_error(buf);
		}
	if(!strcmp(buf, weapon)) break;
	}
	fclose(fpr);

	fpr = fopen(ammunition_file, "r");
	if(fpr == NULL)
        {
        G_fatal_error("can't open ammunition file");
        } 

	while(1)
	{
	if(fscanf(fpr, "%10s%10lf%10lf%10lf",
		buf, &DIAMETER, &MASS, &vel_initial) == EOF)
		{
	sprintf(buf, "Ammunition not listed in database");
	G_fatal_error(buf);
		}
	if(!strcmp(buf, ammunition)) break;
	}
	fclose(fpr);

	TOL_X = window.ns_res / 10.0;

	DIAMETER /= 1000.0;	/* conversion to meters		*/
	MASS /= GRAVITY;       /* conversion from weight to mass*/

	/* conversion of gun angles from degrees to radians     */
        low_angle = low_angle * PI / 180.0;
        high_angle = high_angle * PI / 180.0;

        /* calculate rows and columns of the database window    */
        nrows = G_window_rows();
        ncols = G_window_cols();
 
        len = sizeof(CELL);
        /* decide the rows and columns in every segment         */
        srows = nrows/5 + 1;
        scols = ncols/5 + 1;

        /* allocation of buffer for row i/o     */
        cell = G_allocate_cell_buf();
 
        /* open elevation layer for reading     */
    	old = G_open_cell_old (elev_layer, old_mapset);
    	if (old < 0)
    	{
        char buf[200];
   	sprintf (buf, "%s in %s - can't open cell file", 
					elev_layer, old_mapset);
        G_fatal_error (buf);
        exit(1);
    	}

        /* creating and opening of output layer for writing     */
    	new = G_open_cell_new (out_layer);
    	if (new < 0)
    	{
        sprintf (buf, "%s - can't create cell file", out_layer);
        G_fatal_error (buf);
        exit(1);
    	}
 
        /* creating and formatting of the segmented elev overlay*/
        in_fd = creat("in_seg_file",0666);
        segment_format(in_fd,nrows,ncols,srows,scols,len);
        close(in_fd);
 
      	/* creating and formatting of segmented output overlay	*/
        out_fd = creat("out_seg_file",0666);
        segment_format(out_fd,nrows,ncols,srows,scols,len);
        close(out_fd);
 
        in_fd = open("in_seg_file",2);
        segment_init(&seg_in,in_fd,4);
 
        out_fd = open("out_seg_file",2);
        segment_init(&seg_out,out_fd,4);
 
        /* reading of data from the original elev overlay and   */
        /* writing into the segmented file format               */
        for(row = 0; row < nrows; row++)
        {
        if (G_get_map_row (old,cell,row) < 0)
        exit(1);

        segment_put_row(&seg_in,cell,row);
        }

        /* calculate the map array coordinates of the firing    */
        /* point. (origin at top left of overlay)               */
        row_firept = (window.north - north)/window.ns_res;
        col_firept = (east - window.west)/window.ew_res;

        /* calculate the absolute elevation of the gun  */
        value = (char *) &firept_elev;
        segment_get(&seg_in,value,row_firept,col_firept);
        firept_elev += weapon_elev;
 
        /* tentative maximum range of fall to limit points      */
        /* to be picked up int the preliminary list             */
        tent_max_dist = 1.1 * vel_initial * vel_initial / GRAVITY;
 
        head = NULL;
        total = 0;
 
	/* make a list of pts in the firing zone		*/
        for(i=0; i<nrows; i++){         /* loop over rows       */
                for(j=0; j<ncols; j++){ /* loop over columns    */
 
        if(i==row_firept && j==col_firept);
        else
        head = make_list(head,row_firept-i,j-col_firept,&seg_in);
                                        }
                                }

	/* sort this list in order of decreasing distance from	*/
	/* the firing pt					*/
        head = sort_list(total,head);

	/* eliminate pts that cannot be hit by a shell		*/
	head = unhittable_pts_elimination(head, seg_in);

 
	/* calculate max initial angle of fire			*/
        SEARCH_PT = head;

        while(SEARCH_PT != NULL)
        {
         if(fabs(SEARCH_PT_HIGH_ANGLE) > max_vert_angle)
         max_vert_angle = fabs(SEARCH_PT_HIGH_ANGLE);
         SEARCH_PT = NEXT_SEARCH_PT;
         }

        factor = decide_color_range
			(max_vert_angle / D_TO_RAD_CONV);
 
	/* mark impact pts on the output map			*/
        mark_impact_points(head,&seg_out, row_firept, col_firept,
		low_angle, high_angle, factor, COLOR_SHIFT);
 
	/* mark firing pt					*/
	value = (char *) &data;
	data = 50;
        segment_put(&seg_out,value,row_firept,col_firept);

        segment_flush(&seg_out);

        for(row=0; row< nrows; row++)
        {
        segment_get_row(&seg_out,cell,row);
         
        if(G_put_map_row(new,cell,row) < 0)
        {
        exit(1);
        }
        }
         
        segment_release(&seg_in);
        segment_release(&seg_out);
        close(in_fd);
        close(out_fd);
        G_close_cell(old);
        G_close_cell(new);
        system("rm in_seg_file");
        system("rm out_seg_file");
 

         /*      creating colours for output map         */
	G_read_range(out_layer, current_mapset, &range);
	G_make_color_wave(&colors,range.pmin, range.pmax);
        G_write_colors (out_layer,current_mapset,&colors);
 
}       /* END OF MAIN PROGRAM  */



double decide_color_range(angle)
        double angle;
        {
                int i;
 
                i = angle + 0.99;
                return((COLOR_MAX - COLOR_SHIFT)/i);
                                }
         
/****************************************************************/
/*								*/
/*	This function converts an azimuth angle to a radian	*/
/*	angle measured counterclockwisely from the positive 	*/
/*	x-axis direction					*/

double azimuth_to_angle(string)
		char *string;
{
	double angle;

	if (strlen(string) != 4)
	{
	sprintf(buf, "Illegal azimuth angle - %s", string);
	G_fatal_error(buf);
	}

	sscanf(&string[1], "%2lf", &angle);	

	if (angle > 90.0)
	{
	sprintf(buf, "Illegal azimuth angle - %s", string);
        G_fatal_error(buf); 
        } 

	switch(string[0])
	{
	case 'N': if (string[3] == 'W')
			return((90.0 + angle)*D_TO_RAD_CONV);
		  else if (string[3] == 'E')
			return((90.0 - angle)*D_TO_RAD_CONV);
		  break;

	case 'S': if (string[3] == 'W')
			return((270.0 - angle)*D_TO_RAD_CONV);
		  else if (string[3] == 'E')
			return((270.0 + angle)*D_TO_RAD_CONV);
		  break;
	
	default : break;
	}

	/* in case of different format , print error msg	*/
	sprintf(buf,  "Illegal azimuth angle - %s", string);
        G_fatal_error(buf); 

}
