/* 
 * $Id$
 * Changes by PWC 2/6/95  from NRCS
 * 1.0  2/26/91
 *
 *  Created by R.L.Glenn
 *  USDA, Soil Conservation Service
 *
 *  Input arguements:
 *        s.to.vect input=site_list file to read
 *                  output=vector (digit) file to create
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
    int type, vect_read, ier, count, index;
    int alloc_points, n_points, dims, dbls, strs ;
    double east, north;
    double *xarray, *yarray;
    FILE *fopen(), *attr, *site, *cats, *tmp ;
    struct Option *sitein, *outvect, *opt_desc;
    struct Cell_head window;
    struct GModule *module;
    struct Map_info Map;
    struct line_pnts *Points ;
    Site *s;
	RASTER_MAP_TYPE map_type; 

    sitein = G_define_option();
    sitein->key          = "input";
    sitein->description  = "Name of site input file";
    sitein->type         = TYPE_STRING;
    sitein->required     = YES;
    sitein->multiple     = NO;
	sitein->gisprompt    = "old,site_lists,site list";

    outvect = G_define_option();
    outvect->key          = "output";
    outvect->description  = "Name of vector output file";
    outvect->type         = TYPE_STRING;
    outvect->required     = YES;
    outvect->multiple     = NO;
    outvect->gisprompt    = "new,dig,vector";

    opt_desc = G_define_option();
    opt_desc->key         = "desc";
    opt_desc->description = "String field in site_list to use in vector file";
    opt_desc->type        = TYPE_INTEGER;
    opt_desc->required    = NO;
    opt_desc->answer      = "1";
	
    G_gisinit(argv[0]);
    
    module = G_define_module();
    module->description =        
                    "Converts a GRASS site_lists file into a vector file.";
                    
    if (G_parser(argc, argv)) exit(-1);

    G_get_window (&window);

    new_screen();
    sprintf(path,"%s/%s",G_location_path(),G_mapset());
    N_path = path;

    sprintf(map_name,"%s",outvect->answer);
    sprintf(sname,"%s",sitein->answer);

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
	sprintf(buf,"Site file file [%s] not available\n",sname);
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

    if ((site = fopen (N_site_file, "r+")) == NULL)
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
	G_fatal_error("Creating new vector file.\n") ;
	exit(-1) ;
    }

	/* added for string field description */
    index = atoi(opt_desc->answer);
    if(index <= 0) {
        G_warning("Got field number less than 1 for string attribute.\n"
                                        "Setting to 1...\n");
        index = 1;
    }
    index--;
	
    get_head_info(&(Map.head));

    fprintf (stderr,"\n\ntransfering sites to vect file      \n");

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

    /******** Modified for 5.0 sites API EGM 10/2000 ********/
    count = 0;
    if (G_site_describe(site, &dims, &map_type, &strs, &dbls) != 0) {
        G_fatal_error("Unable to guess site_list format!\n");
    }
    s = G_site_new_struct(map_type, dims, strs, dbls);
    
    if ((index + 1) > s->str_alloc) {
        if(s->str_alloc <= 0) {
            G_warning("No string attributes in site_list\n");
            index = -1;
        }
        else {
            index = 0;
            G_warning("String attribute index out of range.\n"
                                    "Using first attribute instead.\n");
        }
    }
    while (G_site_get(site,s) >= 0) {
        xarray[1] = xarray[0] = s->east;
        yarray[1] = yarray[0] = s->north;
        count++;
        
        switch (s->cattype) {
            case CELL_TYPE:
                fprintf(attr,"P %15.6f %15.6f %d\n",
                                        s->east, s->north, s->ccat);
                fprintf(tmp, "%d:", s->ccat);
                break;
            case FCELL_TYPE:
                fprintf(attr,"P %15.6f %15.6f %-15.6f\n",
                                        s->east, s->north, s->fcat);
                fprintf(tmp, "%f:", s->fcat);
                break;
            case DCELL_TYPE:
                fprintf(attr,"P %15.6f %15.6f %15.6f\n",
                                        s->east, s->north, s->dcat);
                fprintf(tmp, "%f:", s->dcat);
                break;
            default:
                fprintf(attr,"P %15.6f %15.6f %d\n",
                                        s->east, s->north, count);
                fprintf(tmp, "%d:", count);
        }

        if (index >= 0) {
            fprintf(tmp, "%s\n", s->str_att[index]);
        }
        else {
            fprintf(tmp, "\n");
        }

                 /* make a vector dig record */
        if (0 > Vect_copy_xy_to_pnts (Points, xarray, yarray, n_points))
            G_fatal_error ("Vect_copy error\n");
        Vect_write_line (&Map,  (unsigned int) type, Points);
    }
    fprintf(stderr,"\n");
    fclose(attr);
    fclose(site);
    fclose(tmp);
    Vect_close (&Map);
	G_site_free_struct(s);

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
    fprintf(cats,"# %d categories\n%s\n\n0.00 0.00 0.00 0.00\n0:no data\n",
					count,N_name);
    while (fgets (buf, sizeof(buf), tmp) != NULL)
        fprintf(cats,"%s",buf);

    fclose(cats);
    fclose(tmp);
    sprintf( command, "%s/etc/v.build map=%s thresh=no",G_gisbase(), N_name) ;
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
/* vim: softtabstop=4 shiftwidth=4 expandtab */
