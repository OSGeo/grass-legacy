/* $Id$ 
 *
 * Update 12/99 Markus Neteler for GRASS 5
 */
/*
s.to.rast - this program was created to provided a command-line operability
of what could have been done interactively with the s.menu program. Thus,
much of what you find here is simply a modified version of the sites_to_rast
portion of the s.menu code.

This program reads a site file and creates a raster map representation
of the data. If the site file has category descriptions for all the sites
(and the -s flag is not used), the resulting map will reflect the categories
and their text descriptions (if any). This is why the sites list is read
twice - once to see if all sites have the categories and to put them in a
list, then once again to actually create the map. If the -s flag is used,
the first reading is by-passed.

Chris Rewerts, U.S. Army Construction Engineering Research Laboratory
rewerts@zorro.cecer.army.mil
May 20, 1993
*/

#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "gis.h"
#include "local_proto.h"

int main (int argc, char *argv[])
{
    FILE *fd;
    Site *s;
    struct Cell_head window;
    struct Categories cats;
    char *layer;
    char *name, *mapset;
    char title_buf[1024];
    char *prev_cat;
    int quad_size;
    int quiet;
    int zero_one;
    int dims, strs, dbls;
    int said_it;
    int temp_fd;
    char *temp_name;
    int rows, cols, row;
    int dec_field;
    int scan_int;
 
    struct Option *input;
    struct Option *output;
    struct Option *size;
    struct Option *given_title;
    struct Option *field;
    struct Flag   *verbose;
    struct Flag   *one_cat;
    void *rast;
    RASTER_MAP_TYPE map_type, real_map_type;

    G_gisinit (argv[0]);

    input = G_define_option();
    input->key          ="input";
    input->description  ="Name of input site list";
    input->type		=TYPE_STRING;
    input->required     =YES;
    input->gisprompt 	="old,site_lists,site list";
 
    output = G_define_option();
    output->key         ="output";
    output->type        =TYPE_STRING;
    output->required    =YES;
    output->gisprompt   ="new,cell,raster";
    output->description ="Name of new raster file";
 
    size = G_define_option();
    size->key           ="size";
    size->description   ="Number of cells to surround site cell";
    size->type 		=TYPE_INTEGER;
    size->required      =NO;

    given_title = G_define_option ();
    given_title->key		="title";
    given_title->required	= NO;
    given_title->type		= TYPE_STRING;
    given_title->description	= "Title for the resulting raster map";

    field = G_define_option();
    field->key = "field";
    field->type = TYPE_INTEGER;
    field->required = NO;
    field->description = "Attribute field number to use for operation";
    field->answer = "1";
 
    verbose = G_define_flag() ;
    verbose->key         = 'q' ;
    verbose->description = "Run quietly";

    one_cat = G_define_flag();
    one_cat->key	 = 's' ;
    one_cat->description = "Create a single-valued (0/1) raster map";

    if(G_parser(argc, argv))
        exit(1);
 
    quiet = verbose->answer;
    zero_one = one_cat->answer;
    name = input->answer;
    layer = output->answer;
    if (given_title->answer)
        sprintf (title_buf, "%s", given_title->answer);

    if (size->answer)
    {
        if (sscanf (size->answer, "%d", &quad_size) != 1 || quad_size < 0)
        {
            fprintf(stderr,
            "\n%s: <%s> is an illegal size option.\n",
            G_program_name(), size->answer);
            G_usage();
            exit(1);
        }
    }
    else
        quad_size = 0;  /* our default - one cell per site */

    scan_int=sscanf(field->answer,"%d",&dec_field);
    if ((scan_int <= 0) || dec_field < 1)
    {
      char msg[256];
      sprintf(msg,"%s: \"%s\" is an incorrect value for attribute field number.\n",
      G_program_name(), field->answer );
      G_fatal_error (msg);
      exit(1);
     }
    dec_field -= 1;
                                      
    if (G_legal_filename(layer) < 0)
    {
        fprintf (stderr, "\n%s: <%s> - illegal name\n", 
        G_program_name (), layer);
        G_usage ();
        exit(1);
    }

    G_get_window (&window);

    if (!quiet)
    {
        fprintf (stdout, "Using size option: %d\n", quad_size);
        if (zero_one)
            fprintf (stdout, "Forcing single-valued (no-data/1) raster map\n");
        fprintf (stdout, "Finding and opening site list...\n");
    }

    mapset = G_find_sites(name, "");
    if (!mapset)
    {
	fprintf (stderr, "\n%s: site list <%s> not found.\n",
	G_program_name(), name);
	exit(1);
    }

    fd = G_fopen_sites_old (name, mapset);
    if (fd == NULL)
    {
        fprintf (stderr, "\n%s: could not open sites file <%s>",
	G_program_name(), name);
	exit(1);
    }
    if(G_site_describe (fd, &dims, &map_type, &strs, &dbls) != 0)
    {
	fprintf (stderr, "\n%s: wrong site format",G_program_name());
    }
    real_map_type = map_type;
    if(dec_field >= dbls){
       fprintf(stderr,"\n");
          G_fatal_error("selected decimal field column no. %d not present in sites list.", dec_field+1);
    }

    if(!quiet)
       fprintf (stderr, "\nSites map Type: %d, Dims: %d, Strs: %d, Dbls: %d\n", map_type, dims, strs, dbls);

    if (!zero_one)
    {
     zero_one = 0; /* don't create binary map */
    }
       
    if(map_type < 0) /* no #cats found */
    {
      if(dbls == 0) /* no %decimal attributes found. Create binary map */
       {
          /* no cats, no dbls */
            G_fatal_error("No #cats, no dbls attributes found.");
       }
       else          /* no #cats found, but %decimal atts existing */
       {
        if (!quiet )
          {
            fprintf (stderr, "No #cats, but decimal attribs found.\n");
            fprintf (stderr, " Creating FP map from doubles attributes\n");
            fprintf (stderr, " Using attribute field no. %d\n", dec_field+1);
          }
         map_type = DCELL_TYPE;
         zero_one = 0;
       }
    }
        
    if(map_type == 0) /* cats found */
    { 
     if (!zero_one) /* if binary map not forced */
     {
        zero_one = 0;
        if(dbls > 0)  /* dbls also found */
        {
         if (!quiet )
          {
            fprintf (stderr, " Creating FP map from doubles attributes\n");
            fprintf (stderr, " Using attribute field no. %d\n", dec_field+1);
          }
         map_type = DCELL_TYPE;
        } 
        else 
        {      /* cats only */
         if (!quiet )
          {
            fprintf (stderr, " Creating CELL map from cats numbers\n");
          }
	 map_type = CELL_TYPE;
	}
      }
    }

    s = G_site_new_struct (real_map_type, dims, strs, dbls); 
    temp_name = G_tempfile();
    temp_fd = creat(temp_name,0660);
    rast = G_allocate_raster_buf(map_type);	
    rows = window.rows;
    cols = window.cols;

    /*  zero out the entire file that will receive data   */
    if (!quiet)
    {
        fprintf (stderr, "\ninput sites map: <%s> in <%s>\n", name, mapset);
        /*fprintf (stdout, "\ncreating empty raster file ...\n");*/
    }

    G_set_null_value(rast, cols, map_type);
    for(row = 0; row < rows; row++)
        if(write(temp_fd, rast, cols * G_raster_size(map_type))!=
					      cols * G_raster_size(map_type))
	       G_fatal_error("error while writing to temp file");

    if (!quiet)
        fprintf (stderr, "output raster map: <%s> in <%s>\n", layer, G_mapset());

/* 
   if the site descriptions are all of the form: #n <label>
   then assign the site to the #n category with label <label>
   otherwise create a 0/1 cell file
*/
    if (!given_title->answer)
        sprintf (title_buf, "Created by %s from %s", G_program_name (), 
        G_fully_qualified_name (name, mapset) );
    G_init_raster_cats (title_buf, &cats);

    if (!quiet)
        fprintf (stdout, "transferring sites to raster file...\n");

    said_it = 0;
    fseek (fd,0L,0);
    while  (G_site_get (fd, s)>=0)
    {
        if(!zero_one)
        {
            switch(map_type)
            {
	      case CELL_TYPE:
	        G_set_raster_value_c(rast, s->ccat, map_type);
	        break;
	      case FCELL_TYPE:
	        G_set_raster_value_f(rast, s->dbl_att[dec_field], map_type); /* updated 12/99 M.N. */
	        break;
	      case DCELL_TYPE:
	        G_set_raster_value_d(rast, s->dbl_att[dec_field], map_type); /* updated 12/99 M.N. */
	        break;
            }		 

            if(s->str_att)
	    {
                prev_cat = G_get_raster_cat(rast, &cats, map_type);
                if (strcmp(prev_cat, "") && strcmp(prev_cat,s->str_att[dec_field]) && !quiet)
                {
                    if (said_it)
                    {
                        fprintf (stderr,
                        "\nNOTE: Category description mismatch -\n");
                        fprintf (stderr,
                        "more than one label has been found for the same category:\n");
                        said_it = 1;
                    }
                    fprintf (stderr,
                    "    Cell Value:           %f\n", 
					G_get_raster_value_d(rast, map_type));
                    fprintf (stderr,
                    "    Previous label found: %s\n", prev_cat);
                    fprintf (stderr,
                    "    Current label found:  %s\n", s->str_att[dec_field]);
                }
	        G_set_raster_cat (rast, rast, s->str_att[dec_field], &cats, map_type);
		fprintf (stdout,"setting raster cats %f %s", *((FCELL *) rast), *s->str_att);
            }
        }
        else /* zero_one */
	    G_set_raster_value_c(rast, (CELL) 1, map_type);
        write_temp (temp_fd, &window, s->east, s->north, quad_size, rast, map_type);
     }  /* while */
        
     if(zero_one)
     {
	 G_set_cat((CELL ) 0, "no data", &cats);
	 G_set_cat((CELL ) 1, "site data", &cats);
     }
	

    /* if (!quiet)
	     fprintf (stdout, "copying temp file to raster file ...\n");*/
    close_temp(temp_fd, layer, temp_name, rows, cols, map_type);
    if (!quiet)
        fprintf (stdout, "creating support files ...\n");
    G_write_cats (layer, &cats);
    if (!quiet)
        fprintf(stdout, "\n<%s> raster file complete. Bye.\n\n", layer);
    exit(0);
}
