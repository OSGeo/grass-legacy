			/********************************/
			/*	r.le.setup/setup.c	*/
			/*				*/
			/*	       2.1		*/
			/*				*/
			/*       07/20/94 version    	*/
			/*				*/
			/*       Programmer: Baker  	*/
			/*       Univ. of Wyoming   	*/
			/********************************/

#include "setup.h"

struct Colors  *colors_old;




				/* SHOW MAIN MENU AND INVOKE THE SETUP
				   ROUTINES */

void set_map( name, name1, name2, window, top, bot, left, right)
char   *name, *name1, *name2;
struct Cell_head    window;
int                 top, bot, left, right;
{
  char    cmd[30], cmd1[30], cmd2[30];
  int     i, j,  btn, d, class, top0, bot0, right0, left0, paint=0;
  double  msc[2];
  static char *opt_smp[9] = {"SETUP OPTIONS:", 
                             "   REGIONS",  "   SAMPLING FRAME", 
			     "   SAMPLING UNITS", "   MOVING-WINDOW",
			     "   GROUP/CLASS LIMITS", "   COLOR TABLE",
			     "   EXIT-SAVE", NULL};

  colors_old = (struct Colors *)G_malloc(1*sizeof(struct Colors));
  G_init_colors(colors_old);
  G_read_colors(name, G_mapset(), colors_old);

  paint_map(name, name1, name2);
  paint = 1;

				/* setup the screen to raster map 
				   coordinate conversion system */

  scr_cell(&window, top, bot, left, right, msc);

  top0 = top;
  bot0 = bot;
  left0 = left;
  right0 = right;

				/* display the menu and instructions */
again:
  if (!paint) {
     if (G_yes("\n    Refresh the screen before choosing more setup?  ", 1))
        paint_map(name, name1, name2);
  }

  printf("\n\n    USE THE LEFT MOUSE BUTTON TO CHOOSE THE SETUP OPTION OR EXIT\n");

  R_open_driver();
  d = D_popup(5, 1, 3, 0, 0, 4, opt_smp);
  R_close_driver();

/*printf("1t=%d b=%d l=%d r=%d\n",top,bot,left,right);
*/
				/* setup regions */
  if (d == 1)
     set_rgn(msc, name, name1, name2);
				/* setup the sampling frame */
  else if (d == 2) {
     top = top0;
     bot = bot0;
     right = right0;
     left = left0;
     set_frame(msc, &top, &bot, &left, &right);

/*printf("2t=%d b=%d l=%d r=%d\n",top,bot,left,right);
*/
  }
				/* setup sampling units */

  else if (d == 3) {
     backup_file("units");
     sample(top, bot, left, right, name, name1, name2, msc);
  }
				/* setup the moving window */

  else if (d == 4) {
     backup_file("move_wind");
     mov_wind(top, bot, left, right, name, name1, name2, msc);
  }
				
				/* setup group/class limits */

  else if (d == 5)
     ask_group();

				/* change color tables */

  else if (d == 6)
     change_color(name, name1, name2);

				/* reset the colortable and exit */

  else if (d == 7) {
     G_write_colors(name, G_mapset(), colors_old);
     G_free_colors(colors_old);
     exit(0);
  }
  paint = 0;
  goto again;
}




				/* REDISPLAY THE RASTER MAP AND THE
				   OVERLAYS */

void paint_map(n1, n2, n3)
char *n1, *n2, *n3;
{
  char  *cmd;

  cmd = G_malloc(120);

  G_system("clear");
  sprintf(cmd, "d.rast %s", n1);
  G_system("d.erase");
  G_system(cmd);
  if(n2){
     sprintf(cmd, "d.vect %s color=black", n2);
     G_system(cmd);
  }
  if(n3){
     sprintf(cmd, "d.sites sitefile=%s color=black", n3);
     G_system(cmd);
  }
  free(cmd);
}




				/* CHANGE THE COLORTABLE OF THE RASTER
				   MAP */

void  change_color(name, name1, name2)
char *name, *name1, *name2;
{
  struct Colors colors;
  struct Range  range;
  int           d;
  static char *opt_color[10] = {"SEL. NEW COLOR TYPE:", 
                             "   ASPECT",  "   GREY SCALE", 
                             "   RAMPS", "   RAINBOW", 
		  	     "   RANDOM", "   RED-YEL-GRN", 
			     "   WAVE", "   SET OLD", NULL};
  static char *opt_ask[6] = {"WHAT'S NEXT:", "   COLOR TYPE MENU", 
			    "   MAIN MENU", "   SAVE COLOR TYPE", 
			    "   QUIT SETUP",  NULL};
   
  G_read_range(name, G_mapset(), &range);
  G_system("clear");

again:
  printf("\n\n    USE THE MOUSE TO SELECT THE COLOR TYPE.\n");

  R_open_driver();
  d = D_popup(5, 1, 3, 0, 0, 4, opt_color);
  R_close_driver();
  if(d == 1){
      G_make_aspect_colors(&colors, range.pmin, range.pmax);
      G_write_colors(name, G_mapset(), &colors);
  }
  else if(d == 3){
      G_make_color_ramp(&colors, range.pmin, range.pmax);
      G_write_colors(name, G_mapset(), &colors);
  }
  else if(d == 5){
      G_make_random_colors(&colors, range.pmin, range.pmax);
      G_write_colors(name, G_mapset(), &colors);
  }
  else if(d == 6){
      G_make_red_yel_grn(&colors, range.pmin, range.pmax);
      G_write_colors(name, G_mapset(), &colors);
  }
  else if(d == 7){
      G_make_color_wave(&colors, range.pmin, range.pmax);
      G_write_colors(name, G_mapset(), &colors);
  }
  else if(d==2){
      G_make_grey_scale(&colors, range.pmin, range.pmax);
      G_write_colors(name, G_mapset(), &colors);
  }
  else if(d==4){
      G_make_rainbow_colors(&colors, range.pmin, range.pmax);
      G_write_colors(name, G_mapset(), &colors);
  }
  else if(d==8){
      G_write_colors(name, G_mapset(), colors_old);
  }

  paint_map(name, name1, name2);

  R_open_driver();
  d = D_popup(5, 1, 3, 0, 0, 4, opt_ask);
  R_close_driver();
  if(d == 1)   goto again;
  else if (d == 2) return;
  else if (d == 3)
     *colors_old = colors;
  else if (d == 4){
     G_write_colors(name, G_mapset(), colors_old);
     G_free_colors(colors_old);
     exit(0); 
  } 
}



				/* SETUP THE SAMPLING FRAME */

void  set_frame(msc, t, b, l, r)
double  *msc;
int	*t, *b, *l, *r;
{
int	t0, b0, l0, r0, btn;

				/* record the initial boundaries of the map */

  t0 = *t;
  b0 = *b;
  l0 = *l;
  r0 = *r;

				/* if the total area to be sampled will be the
				   whole map */

  G_system("clear");

  if (G_yes("\n    Will the sampling frame (total area within which sampling\n      units are distributed) be the whole map?   ",1))  {
     R_open_driver();  
     R_standard_color(D_translate_color("grey"));
     draw_box(*l, *t, *r, *b, 1);
     R_close_driver();
     printf("\n    Sampling frame set to whole map");
  }

				/* if the total area to be sampled is not the 
				   whole map, then have the user draw the
				   area */

  else {
back:
     G_system("clear");
     printf(" \n    OUTLINE SAMPLING FRAME:\n");
     R_open_driver();  
     printf("\n    Please move cursor to the UPPER-LEFT corner of\n");
     printf("       the sampling frame and click any mouse button\n");  
     R_get_location_with_line(0, 0, l, t, &btn);

     printf("\n    Please move cursor to the LOWER-RIGHT corner of\n");
     printf("       the sampling frame and click any mouse button again\n");
     R_get_location_with_box(*l, *t, r, b, &btn);
     if (*l < l0 || *r > r0 || *t < t0 || *b > b0){
	printf("\n    The cursor is outside of the map, try again\n");
        goto back;
     }
     R_standard_color(D_translate_color("grey"));
     *l = (int)((double)((int)(*l * msc[0] + 0.5))/msc[0]);
     *r = (int)((double)((int)(*r * msc[0] + 0.5))/msc[0]);
     *t = (int)((double)((int)(*t * msc[1] + 0.5))/msc[1]);
     *b = (int)((double)((int)(*b * msc[1] + 0.5))/msc[1]);
     draw_box(*l, *t, *r, *b, 1);
     R_close_driver();
     printf("\n    Sampling frame set to area you just drew");
  }
}







				/* SETUP REGIONS */

void  set_rgn(msc, name, name1, name2)
double  *msc;
char    *name, *name1, *name2;
{
  char     reg_name[20];
  int         x0, y0, xp, yp, *x, *y, xstart, ystart, btn, d;
  static char *opt[6] = {" REGION OPTIONS:", "   DRAW MORE","   START OVER",
                                     "   DONE-SAVE", "   QUIT-NO SAVE", NULL};
  static int   pts, rgn_cnt=0;
  FILE         *tmp;
  char         *tempfile;

				/* get the name of the regions map */
  
  if(!G_ask_cell_new("    ENTER THE NEW REGION MAP NAME:", reg_name))
     return;

				/* allocate memory for storing the 
				   points along the boundary of each
				   region */

  x = (int *)G_malloc(100*sizeof(int));
  y = (int *)G_malloc(100*sizeof(int));

  tempfile = G_tempfile();  
  tmp = fopen(tempfile, "w");
  
  				/* ask the user to outline a region */
  R_open_driver(); 
back:
  G_system("clear");
  ppoint(NULL, 0, 0, -1);
  printf("    PLEASE OUTLINE REGION # %d\n", (++rgn_cnt));
  pbutton(0);
  pts = 0;
  x0 = 0;   y0 = 0;

				/* get the points along the boundary
				   of the region as they are drawn */

  do {
     btn = 0;
     R_standard_color(D_translate_color("white"));
     R_get_location_with_line(x0, y0, &xp, &yp, &btn);
     if(btn == 1)
    	ppoint(msc, xp, yp, 0);
     else if( btn == 2){
	if(!pts){
	  pbutton(1);
	  R_move_abs(xp, yp);
	  xstart = xp;
          ystart = yp;
	}
	x[pts] = xp*msc[0];
   	y[pts] = yp*msc[1];
	ppoint(msc, xp, yp, (++pts));  	         
	x0 = xp;   y0 = yp;	 
    	R_cont_abs(x0, y0);     	
    } else if( btn == 3 && pts < 3)
        printf("\n  Sorry, you have to define more than 2 points\n\n");
  } while(btn != 3 || pts < 3);
  R_cont_abs(xstart, ystart);
  x[pts] = x[0];
  y[pts] = y[0];
  pts++ ;


				/* redisplay the menu and find out what
				   to do next */

  d = D_popup(5, 1, 3, 0, 0, 4, opt); 

				/* save the region and draw another */
 
  if(d == 1) {
     save_rgn(reg_name, tempfile, tmp, x, y, pts, rgn_cnt, 1); 
     goto back;
  } 

				/* start over */

  else if (d == 2) {
     fclose(tmp);
     if (!(tmp = fopen(tempfile, "w")))
         G_fatal_error("Can't open temporary file for storing region info\n");
     rgn_cnt = 0;   
     R_close_driver();
     
     paint_map(name, name1, name2);
     R_open_driver();
     goto back;
  } 

				/* save the region and exit */

  else if(d == 3)
     save_rgn(reg_name, tempfile, tmp, x, y, pts, rgn_cnt, 2);

  R_close_driver();
  free(x);
  free(y);
  unlink (tempfile);

  if(d == 4)
     exit(0);
}





				/* SHOW THE CURSOR COORDINATES TO
				   THE USER */

void ppoint(m, x, y, num)
double  *m;
int     x, y, num;
{
  register int i;

  if(num < 0){
    for(i=0; i<80; i++)  fprintf(stdout, " ");
    fflush(stdout);
  } else {
    if(num > 0)
      fprintf(stdout, "  CELL pt[%d]: ROW %5d,  COL %5d", num, (int)(y*m[1]), (int)(x*m[0]));
    else if(num ==0)
      fprintf(stdout, "  CELL: ROW %5d,  COLS %5d", (int)(y*m[1]), (int)(x*m[0]));

    for(i=0; i<80; i++)  fprintf(stdout, "\b");
    fflush(stdout);
  }
}




				/* PRINT THE INSTRUCTIONS FOR USING THE
				   MOUSE BUTTONS */

void  pbutton(opt)
int opt;
{
  static char *str[2] = {"start", "next"};
  printf("\n    Use the mouse to outline the regions\n");
  printf("       Left button:     where am I?\n");
  printf("       Middle button:   Mark %s point\n", str[opt]);
  printf("       Right  button:   Finish region-connect to 1st point\n\n");
}





				/* SAVE THE REGION */

void save_rgn(name, tempfile, tmp, x, y, pts, class, opt)
char    *name, *tempfile;
FILE    *tmp;
int     pts, *x, *y, class, opt;
{
  char               *cmd;
  struct Cell_head   wind;
  struct Colors	     colors;
  static double	     rx, ry, ofy, ofx;
  register int       i;

				/* setup the temporary file to save
				   the region boundary pts */

  if (class == 1){  
     G_get_set_window(&wind);
     print_hd(tmp, &wind);
     ofy = wind.north; 	ofx = wind.west;
     ry = (wind.north - wind.south)/wind.rows;
     rx = (wind.east - wind.west)/wind.cols;
  }
  fprintf(tmp,"A %10.2f %10.2f %10d\n", (ofy - *y*ry), (*x*rx + ofx), class) ;
  for(i=0; i<pts; i++)	
     fprintf(tmp,"  %10.2f %10.2f\n", (ofy - *(y+i)*ry), (*(x+i)*rx + ofx) ) ;
 
				/* if the choice was made to draw more
				   regions, then return */
 
  if (opt != 2) return;

  fprintf(tmp, "E\n");
  fclose(tmp);
  G_get_set_window(&wind);
  G_put_cellhd(name, &wind);

  				/* make a GRASS raster file from the
				   region boundary pts, using the 
				   poly_to_bmif and bmif_to_cell
				   programs */

  cmd = G_malloc(200);   
  sprintf(cmd, "%s/bin/poly_to_bmif < %s | sort -t: +0n -1 | %s/bin/bmif_to_cell %s", G_gisbase(), tempfile, G_gisbase(), name);
  printf("    Generating '%s' file... %20c\n\n", name, ' ') ;
  G_system(cmd) ;
  printf("    Done\n") ;
  free(cmd);

				/* set the color table for the regions
				   file to color wave */

  G_init_colors(&colors);
  G_make_color_wave(&colors, 1, class);
  G_write_colors(name, G_mapset(), &colors);

  				/* overlay the region file on the 
				   screen */

  if (!(cmd = malloc(20)))
     G_fatal_error("Can't allocate enough memory\n");  
  R_close_driver();
  sprintf(cmd, "d.rast -o  %s", name);
  G_system(cmd);  
  free(cmd);    
  sleep(4);			/* hold the screen for 4 seconds */
  R_open_driver();
}






				/* SETUP THE HEADER FOR THE REGION
				   FILE */

void print_hd(mapfile, universe) 
FILE *mapfile ;
struct Cell_head *universe ;
{
	fprintf(mapfile,"TITLE:\n") ;
	fprintf(mapfile,"	User created region.\n") ;
	fprintf(mapfile,"ENDT\n") ;
	fprintf(mapfile,"SIZE      %10d %10d\n",
		universe->rows, universe->cols) ;
	fprintf(mapfile,"BOUND     %10.2f %10.2f %10.2f %10.2f\n",
		universe->ns_res, universe->ew_res,
		universe->south, universe->west) ;
	fprintf(mapfile,"VERTI\n") ;
}





				/* SETUP THE CONVERSION BETWEEN SCREEN
				   AND RASTER COORDINATES */

void  scr_cell(wind, top, bot, left, right, m)
struct Cell_head  *wind;
int    top, bot, left, right;
double  *m;
{ 
  m[0] = (double)wind->cols / (right - left);
  m[1] = (double)wind->rows / (bot - top);
}

