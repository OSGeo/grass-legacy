#include "gis.h"

/********************* PARMS FOR PROJ **********************/
# define RAD_TO_DEG     57.29577951308232
typedef struct { double u, v; }  UV;

/**** routines for projection support ********/
static UV (*proj1)();      /* projection function 1*/
static UV (*proj2)();      /* projection function 2*/

do_INV(u,v) double *u, *v;
{
 UV data;    /*         data structure supplied to proj conv. routines */
  /*DEBUG   fprintf(stderr,"IN1 %lf %lf\n",*u,*v); */
	 data.u = *u; data.v = *v;
	 data = (*proj1)(data);
	 *u = data.u * RAD_TO_DEG; *v = data.v * RAD_TO_DEG;
/*DEBUG   fprintf(stderr,"OUT %lf %lf\n",*u,*v);*/
}

#define UNIT_FILE "PROJ_UNITS"
#define PROJECTION_FILE "PROJ_INFO"

struct Key_Value *proj_keys;

where_am_i(once, have_spheroid)
{
    char buffer[200] ;
    char buf1[50], buf2[50];
    char temp[100], path[100], cmnd[100];
    double lat, lon ;
    int screen_x, screen_y, status ;
    int cur_screen_x, cur_screen_y ;
    double east, north ;
    int i, button ;
    double D_get_d_north(), D_get_d_south() ;
    double D_get_d_east(), D_get_d_west() ;
    double D_d_to_u_row(), D_d_to_u_col() ;
    int white, black ;
    int projection;

/******************** FOR USGS PROJ CALLS ****************************/
extern UV (*ProjSetup())();
static char *oform = (char *)0; /* output format */
char *p;

    oform = "%.8f";

    projection = G_projection();
    white = D_translate_color("white") ;
    black = D_translate_color("black") ;

    screen_x = ((int)D_get_d_west() + (int)D_get_d_east()) / 2 ;
    screen_y = ((int)D_get_d_north() + (int)D_get_d_south()) / 2 ;

    if (G_projection() != 0 && G_projection() != 3 )
       {
           /* get input projection parameters */
       G__file_name (path, "", PROJECTION_FILE, G_mapset());
       if (access(path,0) != 0)
          {
          sprintf(path,"Mapset %s file not found, run m.setproj\n",
				   PROJECTION_FILE);
          G_fatal_error(path) ;
          }
       proj_keys = G_read_key_value_file(path,&status);
       if (status != 0)
          {
          sprintf(path,"Current mapset %s not found\n",PROJECTION_FILE);
          G_fatal_error(path) ;
          }

           /* build the proj command from data in PROJ_INFO file */
       cmnd[0] = '\0';
       for (i=1; i<proj_keys->nitems-1; i++)
         {
         sprintf(buf1,"+%s=%s\t", proj_keys->key[i],proj_keys->value[i]);
         strcat(cmnd,buf1);
         }
       i = proj_keys->nitems - 1;
       sprintf(buf1,"+%s=%s\0", proj_keys->key[i],proj_keys->value[i]);
       strcat(cmnd,buf1);
             /* compute the lat/long for each corner */
       set_proj(cmnd,1);
       }

    if(!once)
    {
	fprintf(stderr, "\n\nButtons:\n") ;
	fprintf(stderr, "Left:   where am i\n") ;
	fprintf(stderr, "Middle: draw to/from here\n") ;
	fprintf(stderr, "Right:  quit this\n\n\n") ;

	if (projection == PROJECTION_LL)
	    fprintf(stderr,"%18s %18s","LON:","LAT:") ;
	else
	    fprintf(stderr,"%18s %18s","EAST:","NORTH:") ;
	if (have_spheroid)
	    fprintf(stderr," %18s %18s","LON:","LAT:") ;
	fprintf (stderr, "\n");
    }

    do
    {
	R_get_location_with_pointer(&screen_x, &screen_y, &button) ;
	east = D_d_to_u_col((double)screen_x) ;
	north = D_d_to_u_row((double)screen_y) ;
	G_format_easting  (east,  buf1, projection);
	G_format_northing (north, buf2, projection);
	if (once)
	{
	    printf("%18s %18s %d", buf1, buf2, button) ;
	    return(0) ;
	}
	sprintf(buffer,"%18s %18s", buf1, buf2) ;
	if (have_spheroid)
	   {
           if (G_projection() != 0 && 
	       G_projection() != 3 && 
	       G_projection() != 1)
	    {
	    lat = north; lon = east;
            do_INV(&lon,&lat);
	    lat = lat * 3600.;
	    lon = lon * 3600.;
/*-->*/	    }
           else
	    {
	    CC_u2ll_north (north);
	    CC_u2ll (east, &lat, &lon);
	    }
	   CC_lon_format (lon, buf1);
	   CC_lat_format (lat, buf2);
	   sprintf (temp, " %18s %18s", buf1, buf2);
	   strcat (buffer, temp);
	   }
	show (buffer);
	if(button == 3)
	{
	    fprintf (stderr, "\n");
	    return(0) ;
	}
    } while (button != 2) ;

    R_move_abs(screen_x, screen_y) ;
    cur_screen_x = screen_x ;
    cur_screen_y = screen_y ;

    do
    {
	R_get_location_with_line(cur_screen_x, cur_screen_y, &screen_x, &screen_y, &button) ;
	east = D_d_to_u_col((double)screen_x) ;
	north = D_d_to_u_row((double)screen_y) ;
	G_format_easting  (east,  buf1, projection);
	G_format_northing (north, buf2, projection);
	sprintf(buffer,"%18s %18s", buf1, buf2) ;
	if (have_spheroid)
	   {
/*-->*/    if (G_projection() != 0 && 
	       G_projection() != 3 && 
	       G_projection() != 1)
	    {
	    lat = north; lon = east;
            do_INV(&lon,&lat);
	    lat = lat * 3600.;
	    lon = lon * 3600.;
/*-->*/	    }
           else
	    {
	    CC_u2ll_north (north);
	    CC_u2ll (east, &lat, &lon);
	    }
	   CC_lon_format (lon, buf1);
	   CC_lat_format (lat, buf2);
	   sprintf (temp, " %18s %18s", buf1, buf2);
	   strcat (buffer, temp);
	   }
	show (buffer);
	if(button == 2)
	{
	    black_and_white_line(black, white, screen_x,screen_y,cur_screen_x,cur_screen_y) ;
	    cur_screen_x = screen_x ;
	    cur_screen_y = screen_y ;
	    R_move_abs(cur_screen_x, cur_screen_y) ;
	}
    } while (button != 3) ;
    fprintf (stderr, "\n");
    return 0;
}

static
show (buf) char *buf;
{
    char *b;

    if (!isatty(1))
	printf ("%s\n", buf);
    for (b = buf; *b; b++)
	fprintf (stderr, "%c", *b);
    for (b = buf; *b; b++)
	fprintf (stderr, "\b");
}

set_proj(cmnd,n)
char *cmnd;
int n;
{
char *p;
/*DEBUG fprintf(stderr,"\nSET_UP __ %s\n",cmnd);*/
        p = strtok(cmnd," \t");
        load_opt(p, 0);
        while ((p = strtok(NULL," \t")) != NULL)
                load_opt(p, 0);
        proj1 = ProjSetup(1);
/*      if (n == 1) proj1 = ProjSetup(1);
        else proj2 = ProjSetup(0);*/
}
