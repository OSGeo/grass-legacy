/****************************************************************/
/*								*/
/*   range.place
 *
 *   Usage:  range.place
 *
 *   Interactive graphic program for range placement.
 */

#	include <math.h>
#	include <stdio.h>
#	define MAIN 
#	include "opt_new.h"
#	include "graphics.h"
#	include "gis.h"
#	include "dlg.h"
#	include "menu.h"
#	include "dlg_conv.h"
#	define NULL 0
#	include "radians.h"
static int debug = 0 ;
#define DEBUG_(X)	if (debug) fprintf(stderr,X)

double firingpt_x,firingpt_y;
double vel_initial;
double azimuth1, azimuth2;
double high_angle,low_angle;

double ob_elev = 0.0;
/* elevation of observer above ground	*/
double m_dist  = 0.0;
/* max distance for los analysis	*/
double viewpt_x=0.0,
viewpt_y=0.0;    /* viewing pt.'s utm coordinates	*/

/****************************************************************/
FILE *fpattern;
struct dlg dlg;  /*structure to hold dlg-file info 	*/
double *node_coor_info;
double *area_coor_info;
double **line_coor_info;
double *line_bounds_info;
int *n_coors_info;

/*	used for transformation of coordinates		*/
double sin_theta,cos_theta,
rotation,translation_x,translation_y;

/*  orientation of the vertical angle of the pattern	*/
double angle_vert_axis;

/*	coordinates of the origin of the pattern (UTM)	*/
double origin_x, origin_y;
/****************************************************************/


/* flag to indicate whether to display or delete layer 	*/
/* 0 signifies delete , 1 signifies display		*/
int display_delete_flag;

/* name of map displayed in display window		*/
/* if no map is present , value equal to NULL marker	*/
char displayed_map[25] = "";

/* displayed map pointer				*/
struct box *displayed_map_pointer;

/* name of displayed pattern file			*/
char displayed_pattern[25] = "";


/* variable for keeping track of the various no of maps	*/
/* or patterns						*/
int perm_cell_count,user_cell_count;
int basic_patt_count,land_patt_count;


int layer_count;	/* no of files read from a dir	*/

/* pointers to various parent windows of maps or patterns */
struct box *perm_cell_ptr, *user_cell_ptr;
struct box *basic_patt_ptr, *land_patt_ptr;


struct box *ahead;
int screen_x,screen_y;
int iv= -1,jv= -1,ifir= -1 ,jfir= -1;
int scr_top, scr_bot, scr_left, scr_rite, txt_a ,txt_b ;

int button,letter_height,letter_width,throw_text();
struct box *base,talk;
char user_window[25], display[25], path[500],buf[500], 
out_layer[25] ;


typedef struct box *  (*pvoidfn)();
struct Cell_head cellhd,window;
struct box *go_to_child(),*quit(),*write_box_text(),
*draw_ring(),*erase_ring(),*go_to_parent(),*draw_map(),
*erase_polygon(),*draw_polygon(),*return_to_base(),
*erase_display(),*viewpt_text(),*erase_in_poly(),
*output_map(),*viewpt_mouse(),
*free_child_ring(),*scroll_enter(),*scroll(),
*read_from_dir(),*set_conv(),*b(),
*display_flag(),*delete_flag(),*obs_elev(),
*max_dist(),*los(), 
*store_landpattern(), *locate_pattern(),
*refresh_win(), 
*display_pattern(),
*update_user_cell(), *update_landpatt(),
*firingpt_text(), *firingpt_mouse(),*velocity(),
*traj(),*azimuth(),*altitude(),*northing(),
*southing(),*easting(),*westing(),*adj_window(),
*resolution(), *draw_scroll_triangle(),*message();

static pvoidfn dispatch[]=
{
	go_to_child,		/*  0 */
	quit,			/*  1 */
	write_box_text, 	/*  2 */
	erase_in_poly,		/*  3 */
	draw_ring,		/*  4 */
	erase_ring, 		/*  5 */
	viewpt_text,   		/*  6 */
	go_to_parent,		/*  7 */
	draw_map,               /*  8 */
	erase_display,		/*  9 */
	return_to_base,		/* 10 */
	locate_pattern,		/* 11 */
	obs_elev,		/* 12 */
	output_map,		/* 13 */
	viewpt_mouse,           /* 14 */
	display_pattern,	/* 15 */
	free_child_ring,	/* 16 */
	scroll_enter,		/* 17 */
	scroll,			/* 18 */
	store_landpattern,	/* 19 */
	set_conv,		/* 20 */
	read_from_dir,		/* 21 */
	display_flag, 		/* 22 */
	delete_flag,		/* 23 */
	max_dist,		/* 24 */
	los,			/* 25 */
	refresh_win,		/* 26 */
	update_user_cell,	/* 27 */
	update_landpatt,	/* 28 */
	firingpt_text,		/* 29 */
	firingpt_mouse,		/* 30 */
	velocity,		/* 31 */
	traj,			/* 32 */
	azimuth,		/* 33 */
	altitude,		/* 34 */
	northing,		/* 35 */
	adj_window,		/* 36 */
	southing,		/* 37 */
	easting,		/* 38 */
	westing,		/* 39 */
	resolution, 		/* 40 */
	draw_scroll_triangle,	/* 41 */
	message			/* 42 */
};


main(argc,argv) char *argv[];
{
	int n,color,width,height,adjust;
	int no_cur_win ;
	int count ;
	extern char path[];
	char *mapset;
	char initialization_file[100];
	char item[64] ;
	char cur_win[64] ;
	int click_x,click_y;
	struct box *head,*initialize_menu(), 
	*draw_ring(),*find, *hitch, *loop_over_functions();
	FILE *fpr, *fopen();
	int wrapup() ;


	G_gisinit(argv[0]);

	/* put terminal into cbreak mode to allow immediate availability of all imput characters */
	Set_tty() ;
	/* read in the dimensions of the current mapset window  */
	G_get_window(&window);


	/* check if the grid cell of the database window is square */
	if(window.ns_res != window.ew_res)
		G_fatal_error("Grid cell not square. Adjust resolution.");

	out_layer[0]  = '\0';

	system("Derase gray");
	R_open_driver ();


	if (D_get_cur_wind(user_window))
		G_fatal_error("No current window") ;

	D_get_screen_window(&scr_top,&scr_bot,&scr_left,&scr_rite);

	color= D_translate_color("black");
	R_standard_color(color);

	width = scr_rite - scr_left;
	height = scr_bot - scr_top;

	if((100*width) >= (147*height))
	{	
		adjust = (100 * width - 147 * height)/200;
		scr_left += adjust;
		scr_rite -= adjust;
	}

	else
	{	
		adjust = (147 * height - 100 * width)/294;
		scr_top+= adjust;
		scr_bot -= adjust;
	}

	screen_x=(scr_left + scr_rite)/2;
	screen_y=(scr_top + scr_bot)/2;

	sprintf(initialization_file, "%s/etc/range.rc", G_gisbase());
	DEBUG_("kiw\n");
	fpr= fopen(initialization_file,"r");

	head= initialize_menu(NULL,NULL,fpr);
	DEBUG_("kiwi\n");

	head->brother->child->brother->child =	head->brother->child->child;

	DEBUG_("kiwi2\n");
	/* read maps from user's current mapset		*/
	mapset = G_mapset();
	G__file_name(path, "cell", "", mapset);
	DEBUG_("kiwi3\n");
	sprintf(buf,"ls %s",path);
	user_cell_ptr = head->brother->child->child->brother;
	DEBUG_("kiwi1\n");
	read_from_dir(user_cell_ptr);
	user_cell_count = layer_count;

	DEBUG_("kiwin\n");


	/* read names of the placed landpatterns into window strs. */

	G__file_name(path, "landpatterns", "", mapset);
	if(access(path,0)!= 0) 
	G_fatal_error("No database element - landpatterns  - exists");

	sprintf(buf,"ls %s",path);
	land_patt_ptr = head->brother->brother->child->brother;
	read_from_dir(land_patt_ptr);
	land_patt_count = layer_count;


	/* read the names of basic patterns into window strs. */
	sprintf(buf,"ls %s/landpatterns",G_gisbase());
	basic_patt_ptr = head->brother->brother->child;
	read_from_dir(basic_patt_ptr);
	basic_patt_count = layer_count;


	/* read maps from the permanent mapset		*/
	mapset = "PERMANENT";
	G__file_name(path, "cell", "", mapset);
	sprintf(buf,"ls %s",path);
	perm_cell_ptr = head->brother->child->child;
	read_from_dir(perm_cell_ptr);
	perm_cell_count = layer_count;

	find = head;
	hitch = head;

	DEBUG_("kiwaa\n");
	D_new_window(display,raster_y(94),raster_y(7),
	    raster_x(1),raster_x(82));
	color = D_translate_color("white");
	D_show_window(color);
	color = D_translate_color("black");
	R_standard_color(color);

	/* To set up conversions        */
	head = set_conv(head);

	paint_block(raster_y(94)+1,raster_y(7)-1,
	    raster_x(1)+1,raster_x(82));
	letter_width = 4*(head->b-head->t)/9;
	letter_height = 2*(head->b-head->t)/3;
	R_text_size(letter_width,letter_height);

	find= draw_ring(find,hitch);

	talk.l = raster_x(1);
	talk.r = raster_x(82);
	talk.b = raster_y(1);
	talk.t = raster_y(5);
	talk.brother = NULL;
	talk.parent = NULL;
	talk.child  = NULL;
	talk.i = 0;
	talk.func_list = NULL;
	talk.text[0] = 0;
	txt_a= talk.l + (talk.b - talk.t)/3;
	txt_b= talk.t + 3* (talk.b - talk.t)/4;

	color = D_translate_color("white");
	R_standard_color(color);
	outline_box(talk.t,talk.b,talk.l,talk.r);
	color = D_translate_color("black");
	R_standard_color(color);
	paint_block(talk.t+1,talk.b-1,talk.l+1,talk.r-1);

	do
	{
		R_get_location_with_pointer(&screen_x,&screen_y,&button);

		while(screen_x < find->l || 
		    screen_x > find->r ||
		    screen_y < find->t ||
		    screen_y > find->b)

		{
			find = find->brother;
			if(find == hitch) break;
		}

		if(screen_x >= find->l && screen_y >= find->t
		    && screen_x <= find->r
		    && screen_y <= find->b)
		{
			color = D_translate_color("white");
			R_standard_color(color);
			outline_box(find->t-1,find->b+1,find->l-1,find->r+1);
			outline_box(find->t-2,find->b+2,find->l-2,find->r+2);
			color = D_translate_color("black");
			R_standard_color(color);

			find =  loop_over_functions(find,dispatch);

			hitch = find;
		}

		color = D_translate_color("gray");
		R_standard_color(color);
		outline_box(find->t-1,find->b+1,find->l-1,find->r+1);
		outline_box(find->t-2,find->b+2,find->l-2,find->r+2);

	} while( button != 4) ;

	/*
	echo(); 
	nl();
	endwin();	
	*/

	D_set_cur_wind(user_window);
	displayed_map[0] = 'e';
	erase_display(NULL);
	R_close_driver();

	Old_tty() ;
}	/* END OF MAIN PROGRAM */


/*	This function reads all file names from a directory	*/

struct box *read_from_dir(parent)
struct box *parent;
{
	FILE *ls,*popen();
	struct box *pivot, *new, *arrow;
	char *malloc();
	int *int_ptr;
	extern int layer_count;
	extern char buf[];

	arrow = parent->child; 	/* arrow points to "scroll" box	   */
	pivot = arrow->brother;	/* pivot points to "return" box	   */

	ls = popen(buf,"r");

	layer_count=0;

	while(fgets(buf,40,ls))
	{
		layer_count++;
		new = (struct box *) malloc(sizeof(struct box));
		pivot->brother = new;
		sscanf(buf,"%[^\n]",new->text);

		new->parent = parent;

		new->child= NULL;
		new->i = 1;
		int_ptr= (int *) malloc(sizeof(int));

		if(!strcmp(parent->text,"add") || 
		    !strcmp(parent->text,"modify"))
			*int_ptr = 15;
		else *int_ptr = 8;
		new->func_list = int_ptr;

		pivot = pivot->brother;
	}

	pivot->brother=arrow;
	pclose(ls);

	return(parent);
}



struct box *return_to_base(me)

struct box *me;
{
	return(base);
}
