/*
 * $Id$
 * 
 ******************************************************************************
 * MODULE:       s.to.vect -- Convert site_list to a vector point layer.
 * 
 * AUTHOR(S):    Original author (1991) R.L. Glenn 
 *                  - USDA, Soil Conservation Service
 *               Changes (1995) -- PWC (??) from NRCS
 *               Modified (2000-1) Eric G. Miller <egm2@jps.net>
 *               
 * PURPOSE:      A general module to convert site_lists to vector point layers.
 * 	    
 * COPYRIGHT:    (C) 2000 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *               License (>=v2). Read the file COPYING that comes with GRASS
 *               for details.
 *
 ******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
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
    char *sname, *mapset, *cptr;
    char path[512], *map_name, buf[1024], command[512];
    char *ptr;
    int type, vect_read, ier, count, index, field;
    int alloc_points, n_points, dims, dbls, strs ;
    double east, north;
    double *xarray, *yarray;
    FILE   *attr, *site, *cats, *tmp ;
    struct Option *sitein, *outvect, *opt_desc;
    struct Flag   *prompt ;
    struct Cell_head window;
    struct GModule *module;
    struct Map_info Map;
    struct line_pnts *Points ;
    Site *s;
    RASTER_MAP_TYPE map_type; 

    G_gisinit(argv[0]);

    module = G_define_module();
    module->description =        
                    "Converts a GRASS site_lists file into a vector file.";

    prompt = G_define_flag ();
    prompt->key = 'p';
    prompt->description = "Don't prompt for Map header information";
    
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

    opt_desc = G_define_option() ;
    opt_desc->key         = "cat";
    opt_desc->key_desc    = "type,index";
    opt_desc->type        = TYPE_STRING;
    opt_desc->description = "Field type (\"string\", \"dim\", or \"decimal\") "\
        "and index to use for category value (uses \"cat\" by default)";
    opt_desc->required    = NO;
    
                    
    if (G_parser(argc, argv)) exit(-1);

    snprintf(path, 512, "%s/%s", G_location_path(), G_mapset());
    N_path = path;

    map_name = outvect->answer ;
    sname    = sitein->answer  ;

    mapset = G_find_vector (map_name, G_mapset());
    if (mapset != NULL)
	{
	snprintf(buf, 1024, "Vector file [%s] already in mapset [%s]\n",
					 map_name, G_mapset());
	G_fatal_error(buf) ;
	}
    N_name = map_name;

    mapset = G_find_file2 (SITE_DIR,sname,"");
    if (mapset == NULL)
	{
	snprintf(buf, 1024, "Site file file [%s] not available\n",sname);
	G_fatal_error(buf) ;
	}
    S_name = sname;

    /* figure out what to use for category values */
    if (opt_desc->answer != NULL && (cptr = opt_desc->answers[0]) != NULL)
    {
        if (strncmp (cptr, "string", 6) == 0)
        {
            field = SITE_COL_STR;
        }
        else if (strncmp (cptr, "decimal", 7) == 0)
        {
            field = SITE_COL_DBL;
        }
        else if (strncmp (cptr, "dim", 3) == 0)
        {
            field = SITE_COL_DIM;
        }
        else
        {
            G_fatal_error ("Unknown attribute type [%s]", cptr);
        }
        cptr = opt_desc->answers[1];
        index = (int) strtol (opt_desc->answers[1], &cptr, 10) ;
        if (cptr == opt_desc->answers[1])
        {
            G_fatal_error ("Attribute index (%s) conversion to integer failed",
                    opt_desc->answers[1]);
        }
        if (index <= 0)
        {
            G_fatal_error ("Attribute index (%d) must be a positive integer",
                    index);
        }
        else if (index < 3 && field == SITE_COL_DIM)
        {
            G_fatal_error ("Attribute index (%d) for type \"dim\" must be "\
                    "greater than 2", index);
        }
        index--;
        if (field == SITE_COL_DIM)
            index -= 2;
    }
    else    /* Use category value */
    {
        index = -1;
        field = SITE_COL_NUL;
    }

    /* store the original file names */
    {

	snprintf (buf, 1024, "%s/%s/%s", N_path, "dig", N_name);
	N_dig_file= G_store (buf);

	snprintf (buf, 1024, "%s/%s/%s", N_path, "dig_att", N_name);
	N_att_file = G_store (buf);

	snprintf (buf, 1024, "%s/%s/%s", N_path, "dig_cats", N_name);
	N_cat_file = G_store (buf);

	snprintf (buf, 1024, "%s/%s/%s", N_path, "site_lists", S_name);
	N_site_file = G_store (buf);
    }

    if ((site = G_sites_open_old (S_name, mapset)) == NULL)
    {
        snprintf (buf, 1024, "Not able to open site file <%s@%s>\n", 
                S_name, mapset);
        G_fatal_error (buf);
    }

    if (G_site_describe(site, &dims, &map_type, &strs, &dbls) != 0) {
        G_fatal_error("Unable to guess site_list format!\n");
    }
    if ((s = G_site_new_struct(map_type, dims, strs, dbls)) == NULL)
        G_fatal_error ("Failed to allocate site structure");
   
    switch (field)
    {
        case SITE_COL_STR:
            if ((index + 1) > s->str_alloc)
                G_fatal_error ("String attribute with index %d does not exist",
                        index + 1);
            break;
        case SITE_COL_DIM:
            if ((index + 1) > s->dim_alloc)
                G_fatal_error ("Dimension attribute with index %d does not exist",
                        index + 3);
            break;
        case SITE_COL_DBL:
            if ((index + 1) > s->dbl_alloc)
                G_fatal_error ("Decimal attribute with index %d does not exist",
                        index + 1);
            break;
        case SITE_COL_NUL:
            break;
        default:
            G_fatal_error ("Program Error! Failed initialization of attribute "\
                    "field type");
            break;
    }

    /* Can't use floating point categories in vectors ! */
    if (map_type == FCELL_TYPE || map_type == DCELL_TYPE)
    {
        G_warning ("Sites list has floating point category values\n"\
                "Using sequential integer instead\n");
        map_type == -1;
    }
    else if (map_type != CELL_TYPE)
    {
        G_warning ("Site list does not have category values\n"\
                "Using sequential integer instead\n");
    }


    if ( (attr = fopen(N_att_file, "w+") ) == NULL )
    {
        snprintf (buf, 1024, "Not able to open attribute file <%s>\n", 
                N_att_file);
        G_fatal_error (buf);
    }

    temp_file = G_tempfile();
    if ( (tmp = fopen(temp_file, "w+") ) == NULL )
    {
       snprintf (buf, 1024, "Not able to open temporary category file <%s>\n", 
               temp_file);
       G_fatal_error (buf);
    }

                    /* Create new digit file */
    if ((vect_read = Vect_open_new (&Map, map_name)) < 0)
    {
	G_fatal_error("Creating new vector file.\n") ;
	exit(-1) ;
    }

    G_get_window (&window);

    /* Get map info from user, unless they specified no prompt */
    if (prompt->answer)
    {
        set_default_head_info (&(Map.head));
    }
    else
    {
        new_screen();
        get_head_info(&(Map.head));
    }

    fprintf (stderr,"\n\ntransfering sites to vect file      \n");

                    /* Initialize the Point structure, ONCE */
    Points = Vect_new_line_struct();
                    /* allocat for DOT type */
    type = DOT ;
    alloc_points = 3;
    xarray = (double *) dig_falloc(alloc_points, sizeof(double));
    yarray = (double *) dig_falloc(alloc_points, sizeof(double));

    n_points = 2;
    count = 0;
    while (G_site_get(site,s) >= 0) {
        xarray[1] = xarray[0] = s->east;
        yarray[1] = yarray[0] = s->north;
        count++;

        if (s->cattype == CELL_TYPE) 
        {
            fprintf(attr,"P %15.6f %15.6f %d\n",
                                    s->east, s->north, s->ccat);
            fprintf(tmp, "%d:", s->ccat);
        }
        else
        {
            fprintf(attr,"P %15.6f %15.6f %d\n",
                                    s->east, s->north, count);
            fprintf(tmp, "%d:", count);
        }

        switch (field)
        {
            case SITE_COL_NUL:
                if (s->cattype == CELL_TYPE)
                    fprintf (tmp, "%d\n", s->ccat);
                else
                    fprintf (tmp, "%d\n", count);
                break;
            case SITE_COL_DIM:
                fprintf (tmp, "%15.15f\n", s->dim[index]);
                break;
            case SITE_COL_DBL:
                fprintf (tmp, "%15.15f\n", s->dbl_att[index]);
                break;
            case SITE_COL_STR:
                fprintf (tmp, "%s\n", s->str_att[index]);
                break;
            default:
                break;  /* Should've already caught this above */
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
       snprintf (buf, 1024, "Not able to open category file <%s>\n", 
               N_cat_file);
       G_fatal_error (buf);
       }

    if ( (tmp = fopen(temp_file, "r") ) == NULL )
       {
       snprintf (buf, 1024, "Not able to open temporary category file <%s>\n", 
               temp_file);
       G_fatal_error (buf);
       }

                     /* make a category file header */
    fprintf(cats,"# %d categories\n%s\n\n0.00 0.00 0.00 0.00\n0:no data\n",
					count,N_name);
    while (fgets (buf, sizeof(buf), tmp) != NULL)
        fprintf(cats,"%s",buf);

    fclose(cats);
    fclose(tmp);
    snprintf( command, 512, "%s/etc/v.build map=%s thresh=no",
            G_gisbase(), N_name) ;
    ier = system ( command );
    if (ier && 0xff00)
	{
	fprintf(stderr, "ERROR(%s):  Could not build digit file: '%s'\n"
			, "v.build", N_name) ;
	exit(-1) ;
	}

    fprintf (stdout,"\n<%s> vector file complete\n", N_name); 
    
    return 0;
}
/* vim: set softtabstop=4 shiftwidth=4 expandtab: */
