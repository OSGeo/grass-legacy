/* Changes by PWC 2/6/95  from NRCS */

/*  @(#)main.c    1.0  2/26/91  */
/*
**  Created by R.L.Glenn
**  USDA, Soil Conservation Service
**
**  Input arguements:
**        s.to.vect input=site_list file to read
**                  output=vector (digit) file to create
*/
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <ctype.h>
#include "gis.h"
#include "Vect.h"
#include "V_.h"
#include "local_proto.h"

#define MAIN
#define	B_DIG		"dig"
#define	DIG_ATT		"dig_att"
#define	DIG_CAT		"dig_cats"
#define	SITE_DIR	"site_lists"

static char *N_dig_file;
static char *N_att_file;
static char *N_cat_file;
static char *N_site_file;
static char *temp_file;
static char *N_path;
static char *N_name;
static char *S_name;

int main (int argc, char *argv[])
{
    char sname[40], *mapset, desc[256];
    char path[128], map_name[20], buf[200], command[256];
    char *ptr;
    int i, type, vect_read, ier, cat, have_site;
    int alloc_points, n_points ;
    double east, north;
    double *xarray, *yarray;
    FILE *fopen(), *attr, *site, *cats, *tmp ;
    struct Option *sitein, *outvect;
    struct Cell_head window;
    struct Map_info Map;
    struct line_pnts *Points ;

    sitein = G_define_option();
    sitein->key          = "input";
    sitein->description  = "Name of site input file";
    sitein->type         = TYPE_STRING;
    sitein->required     = YES;
    sitein->multiple     = NO;

    outvect = G_define_option();
    outvect->key          = "output";
    outvect->description  = "Name of vector output file";
    outvect->type         = TYPE_STRING;
    outvect->required     = YES;
    outvect->multiple     = NO;
    outvect->gisprompt    = "new,dig,vector";

    G_gisinit(argv[0]);
    if (G_parser(argc, argv)) exit(-1);

    G_get_window (&window);

    new_screen();
    sprintf(path,"%s/%s",G_location_path(),G_mapset());
    N_path = path;

    sprintf(map_name,"%s",outvect->answer);
    sprintf(sname,"%s",sitein->answer);
    have_site = 0;

    mapset = G_find_vector (map_name, G_mapset());
    if (mapset != NULL)
	{
	sprintf(buf,"Vector file [%s] already in mapset [%s]\n",
					 map_name, G_mapset());
	G_fatal_error(buf) ;
	}
    N_name = map_name;

    mapset = G_find_file (SITE_DIR,sname,"");
    if (mapset == NULL)
	{
	sprintf(buf,"Site file file [%s] not available",sname);
	G_fatal_error(buf) ;
	}
    S_name = sname;
    
    /* store the original file names */
    {

	sprintf (buf, "%s/%s/%s", N_path, "dig", N_name);
	N_dig_file= G_store (buf);

	sprintf (buf, "%s/%s/%s", N_path, "dig_att", N_name);
	N_att_file = G_store (buf);

	sprintf (buf, "%s/%s/%s", N_path, "dig_cats", N_name);
	N_cat_file = G_store (buf);

	sprintf (buf, "%s/%s/%s", N_path, "site_lists", S_name);
	N_site_file = G_store (buf);
    }

    if ((site = fopen (N_site_file, "r+")) != NULL)
	have_site = 1;
    else
    {
	sprintf (buf, "Not able to open site file <%s>\n", N_site_file);
	G_fatal_error (buf);
    }

    if ( (attr = fopen(N_att_file, "w+") ) == NULL )
       {
       sprintf (buf, "Not able to open attribute file <%s>\n", N_att_file);
       G_fatal_error (buf);
       }

    temp_file = G_tempfile();
    if ( (tmp = fopen(temp_file, "w+") ) == NULL )
       {
       sprintf (buf, "Not able to open temporary category file <%s>\n", temp_file);
       G_fatal_error (buf);
       }

                    /* Create new digit file */
    if ((vect_read = Vect_open_new (&Map, map_name)) < 0)
	{
	G_fatal_error("Creating new vector file.") ;
	exit(-1) ;
	}

    get_head_info(&(Map.head));

    fprintf (stderr,"\n\ntransfering sites to vect file      ");

                    /* Initialize the Point structure, ONCE */
    Points = Vect_new_line_struct();
                    /* allocat for DOT type */
    type = DOT ;
    alloc_points = 3;
    xarray = (double *) dig_falloc(alloc_points, sizeof(double));
    yarray = (double *) dig_falloc(alloc_points, sizeof(double));

    /*********** removed by PWC 2/6/95 **************
    x = xarray;
    y = yarray;
    ************************************************/

    n_points = 2;
    i = 1;

    while (fgets (buf, sizeof(buf), site) != NULL)
            {
	                    /* skip comment lines */
	    if (!isdigit(*buf) && *buf != '-') continue; 
            fprintf(stderr,"\b\b\b\b\b%5d",i); 
            ptr = buf;
            for (;;)
            {
               while (*ptr == ' ' || *ptr == '\t') ptr++;
               if (*ptr == '\174') *(ptr) = '\040';      /* | */
               if (*ptr == '\043') *(ptr) = '\040';      /* # */
               if (*ptr == '\012' || *ptr == '\015' || *ptr == 0) break;
               ptr++;
            }
            ier = sscanf (buf, "%lf %lf %d %[^\n]", 
                  &east, &north, &cat, desc);

  	    if ( ier < 3)
	        {
	        fprintf (stderr, "\nBad line '%s'\n", buf);
		sleep(2);
                fprintf (stderr,"\n\ntransfering sites to vect file      ");
	        continue;
	        }

                     /* put in x,y coord record */
	    /*********** changed by PWC 2/6/95 ************
            *x = east; xarray[1] = xarray[0];
            *y = north; yarray[1] = yarray[0];
	    **********************************************/
            xarray[1] = xarray[0] = east;
            yarray[1] = yarray[0] = north;

		     /* make an attribute record */
  	    fprintf(attr,"P %15.7f %15.7f%7d\n",east,north,cat); 
                     /* make a category record */

	    /*********** changed by PWC 2/6/95 ***********
            fprintf(tmp,"%d:%s\n",cat,desc);
	    *********************************************/
            fprintf(tmp,"%d:",cat);
            if (ier == 4) fprintf(tmp,"%s",desc);
            fprintf(tmp,"\n");

		     /* make a vector dig record */
	    if (0 > Vect_copy_xy_to_pnts (Points, xarray, yarray, n_points))
	      G_fatal_error ("Vect_copy error");
/*
fprintf(stderr,"\t#points= %d\n",Points->n_points);
for (ier = 0; ier < Points->n_points; ier++)
{
fprintf(stderr,"Points[%d] x= %f, y= %f\n",ier,Points->x[ier],Points->y[ier]);
}
fprintf(stderr,"\twrite line\n");
*/
	    Vect_write_line (&Map,  (unsigned int) type, Points);
	    i++;
	    }
    fprintf(stderr,"\n");
    fclose(attr);
    fclose(site);
    fclose(tmp);
    Vect_close (&Map);

    /************* added by PWC 2/6/95 **************/
    sprintf(buf, "sort -o%s -t: +0n -1 %s", temp_file, temp_file);
    system(buf);
    /************************************************/

    fprintf (stdout,"creating support files ...\n");
    if ( (cats = fopen(N_cat_file, "w+") ) == NULL )
       {
       sprintf (buf, "Not able to open category file <%s>\n", N_cat_file);
       G_fatal_error (buf);
       }

    if ( (tmp = fopen(temp_file, "r") ) == NULL )
       {
       sprintf (buf, "Not able to open temporary category file <%s>\n", temp_file);
       G_fatal_error (buf);
       }

                     /* make a category file header */
    fprintf(cats,"# %d categories\n%s\n\n0.00 0.00 0.00 0.00\n0:no data\n",i-1,N_name);
    while (fgets (buf, sizeof(buf), tmp) != NULL)
            fprintf(cats,"%s",buf);

    fclose(cats);
    fclose(tmp);

    sprintf( command, "%s/etc/v.build map=%s thresh=no",G_gisbase(), N_name) ;

/*  fprintf(stderr,"%s\n",command);   */
    ier = system ( command );
    if (ier && 0xff00)
	{
	fprintf(stderr, "ERROR(%s):  Could not build digit file: '%s'\n"
			, "v.build", N_name) ;
	exit(-1) ;
	}

    fprintf (stdout,"\n<%s> vector file complete\n", N_name); 
    exit (1);
}
