#define MAIN
#include <unistd.h>
#include "gis.h"
#include "mask.h"
#include "local_proto.h"

struct Cell_head cellhd;
static int parse_vallist (char **, d_Mask *);

int main(int argc, char *argv[])
{
    char *name, *mapset;
    char rname[256], rmapset[256];
    int row, col, null_fd;
    char element[300], path[400];
    unsigned char *null_bits;
    RASTER_MAP_TYPE map_type;
    int change_null=0, create, remove, only_int, only_fp, only_null;
    char buf[1024];
    int is_reclass;

	struct GModule *module;
    struct
    {
	struct Option *map;
	struct Option *setnull;
	struct Option *null;
    } parms;
    struct
    {
	struct Flag *f;
	struct Flag *n;
        struct Flag *i;
        struct Flag *c;
        struct Flag *r;
    } flags;

	module = G_define_module();
	module->description =
		"The function of r.null is to explicitly create the NULL-value "
		"bitmap file.";

	parms.map = G_define_option();
	parms.map->key    = "map";
	parms.map->type   = TYPE_STRING ;
	parms.map->required = YES ;
	parms.map->multiple = NO ;
	parms.map->gisprompt  = "old,cell,raster" ;
	parms.map->description = "raster map for which to edit null file";

	parms.setnull = G_define_option();
	parms.setnull->key   = "setnull";
	parms.setnull->key_desc    = "val[-val]";
	parms.setnull->type   = TYPE_STRING ;
	parms.setnull->required = NO ;
	parms.setnull->multiple = YES ;
	parms.setnull->description = "List of cell values to be set to NULL";

        parms.null = G_define_option();
        parms.null->key    = "null";
        parms.null->type   = TYPE_DOUBLE;
	parms.null->required   = NO;
        parms.null->multiple   = NO;
        parms.null->description= "The value to replace the null value by";

	flags.f = G_define_flag();
	flags.f->key = 'f';
	flags.f->description = "Only do the work if the map is floating-point";

	flags.i = G_define_flag();
	flags.i->key = 'i';
	flags.i->description = "Only do the work if the map is integer";

	flags.n = G_define_flag();
	flags.n->key = 'n';
	flags.n->description = "Only do the work if the map doesn't have a NULL-value bitmap file";

	flags.c = G_define_flag();
	flags.c->key = 'c';
	flags.c->description = "create NULL-value bitmap file validating all data cells";

	flags.r = G_define_flag();
	flags.r->key = 'r';
	flags.r->description = "remove NULL-value bitmap file";

        G_gisinit (argv[0]);

	if (G_parser(argc,argv))
		exit(0);

	only_int  = flags.i->answer;
	only_fp   = flags.f->answer;
	only_null = flags.n->answer;
	create    = flags.c->answer;
	remove    = flags.r->answer;

        name = parms.map->answer;
        mapset = G_find_cell2 (name, "");
	if (mapset == NULL)
        {
	    sprintf (buf, "%s - not found", name);
	    G_fatal_error (buf);
        }

        is_reclass = (G_is_reclass (name, mapset, rname, rmapset) > 0);
        if (is_reclass)
        {
            sprintf(buf, "%s is a reclass of another map. Exiting.", name);
			  G_fatal_error(buf);
        }


	if(strcmp(mapset, G_mapset()) != 0)
        {
	    sprintf (buf, "%s is not in your mapset (%s)", name, G_mapset());
	    G_fatal_error (buf);
	}

	if(parms.null->answer)
	{
	   if(sscanf(parms.null->answer, "%lf", &new_null) == 1)
	       change_null =1;
           else
	   {
	     sprintf (buf, "%s is illegal entry for null", 
					  parms.null->answer);
	     G_fatal_error (buf);
           }
        }

	map_type = G_raster_map_type(name, mapset);

        sprintf(element, "cell_misc/%s", name);
        if(only_null && !G_find_file(element, "null", mapset))
	{
	     sprintf (buf, "%s doesn't have null bitmap file! Exiting", name);
	     G_warning(buf);
	     exit(0);
	}

        if(map_type == CELL_TYPE)
	{
	   if(only_fp)
	   {
	     sprintf (buf, "%s is integer! Exiting", name);
	     G_warning(buf);
	     exit(0);
           }

	   if((double)((int) new_null) != new_null)
	   {
	     sprintf (buf, 
		 "%s is an integer map! Using null=%d", name, (int) new_null);
	     G_warning(buf);
	     new_null = (double) ((int) new_null);
           }
        }
	else if(only_int)
	{
	     sprintf (buf, "%s is floating point! Exiting", name);
	     G_warning(buf);
	     exit(0);
	}

        parse_vallist (parms.setnull->answers, &d_mask);

	if (G_get_cellhd (name, mapset, &cellhd) < 0)
	{
	    fprintf (stdout,buf, "Can't read cell header for %s", name);
	    G_fatal_error(buf);
        }

    if(create)
    {
	/* write a file of no-nulls */
	null_bits = (unsigned char *) G__allocate_null_bits (cellhd.cols);
	/* init all cells to 0's */
	for (col = 0; col < G__null_bitstream_size(cellhd.cols); col++)
	     null_bits[col] = 0;

	sprintf(element,"cell_misc/%s",name);
	null_fd = G_open_new (element, "null");
 
	fprintf(stdout, "Writing new null file for [%s]... ", name);
	
	for(row=0; row < cellhd.rows; row++)
	{
	   G_percent (row, cellhd.rows, 1);
	   if( G__write_null_bits(null_fd, null_bits, row, cellhd.cols, 0) < 0)
	   {
	       sprintf(buf, "Error writing null row %d", row); 
	       G_fatal_error(buf);
           }
	}
	G_percent (row, cellhd.rows, 1);
	close(null_fd);
        exit(0);
    }

    if(remove)
    {
	/* write a file of no-nulls */
	fprintf(stdout, "Removing null file for [%s]... ", name);
	sprintf(element,"cell_misc/%s",name);
	null_fd = G_open_new (element, "null");
        G__file_name(path, element, "null", mapset);
	unlink (path);
	fprintf(stdout, "Done.\n");
        exit(0);
    }
    
    process(name, mapset, change_null, map_type);
    exit(0);
}

static int parse_vallist ( char **vallist, d_Mask *d_mask)
{
    char buf[1024];
    char x[2];
    FILE *fd;

    init_d_mask_rules (d_mask);
    if (vallist == NULL) return 1;

    for ( ; *vallist; vallist++)
    {
	if (*vallist[0] == '/')
	{
	    fd = fopen (*vallist, "r");
	    if (fd == NULL)
	    {
		perror (*vallist);
		G_usage();
		exit(1);
	    }
	    while (fgets (buf, sizeof buf, fd))
	    {
		if (sscanf (buf, "%1s", x) != 1 || *x == '#')
		    continue;
		parse_d_mask_rule (buf, d_mask, *vallist);
	    }
	    fclose(fd);
	}
	else
	    parse_d_mask_rule (*vallist, d_mask, (char *)NULL);
    }

    return 0;
}

int 
parse_d_mask_rule (char *vallist, d_Mask *d_mask, char *where)
{
    double a,b;
    char junk[128];

/* #-# */
    if (sscanf (vallist,"%lf-%lf",&a,&b) == 2)
    {
	fprintf (stdout,"adding rule: %f - %f\n", a,b);
	add_d_mask_rule (d_mask, a, b, 0);
    }
/* inf-# */
    else if (sscanf (vallist,"%[^ -\t]-%lf", junk, &a) == 2)
	add_d_mask_rule (d_mask, a, a, -1);

/* #-inf */
    else if (sscanf (vallist,"%lf-%[^ \t]", &a, junk) == 2)
	add_d_mask_rule (d_mask, a, a, 1);

/* # */
    else if (sscanf (vallist,"%lf",&a) == 1)
	add_d_mask_rule (d_mask, a, a, 0);

    else
    {
	if(where)
	    fprintf (stderr, "%s: ", where);
	fprintf (stderr, "%s: illegal value spec\n", vallist);
	G_usage();
	exit(1);
    }

    return 0;
}

int 
process (char *name, char *mapset, int change_null, RASTER_MAP_TYPE map_type)
{
    struct Colors colr;
    struct History hist;
    struct Categories cats;
    struct Quant quant;
    int colr_ok;
    int hist_ok;
    int cats_ok;
    int quant_ok;

    G_suppress_warnings(1);
    colr_ok = G_read_colors (name, mapset, &colr) > 0;
    hist_ok = G_read_history (name, mapset, &hist) >= 0;
    cats_ok = G_read_raster_cats (name, mapset, &cats) >= 0;

    if(map_type != CELL_TYPE)
    {
       G_quant_init(&quant);
       quant_ok = G_read_quant(name, mapset, &quant);
       G_suppress_warnings(0);
    }

    if (doit(name, mapset, change_null, map_type)) return 1;

    if (colr_ok)
    {
	G_write_colors (name, mapset, &colr);
	G_free_colors (&colr);
    }
    if (hist_ok)
	G_write_history (name, &hist);
    if (cats_ok)
    {
	cats.num = G_number_of_cats (name, mapset);
	G_write_raster_cats (name, &cats);
	G_free_cats (&cats);
    }
    if (map_type != CELL_TYPE && quant_ok)
	G_write_quant (name, mapset, &quant);

    return 0;
}

int 
doit (char *name, char *mapset, int change_null, RASTER_MAP_TYPE map_type)
{
    int new, old, row;
    char buf[256];
    void *rast;

    G_set_window (&cellhd);

    old = G_open_cell_old (name, mapset);
    if (old < 0)
    {
       sprintf(buf, ", Can't open %s (old)", name);
       G_fatal_error(buf);
    }

    new = G_open_raster_new(name, map_type);

    if (new < 0)
    {
       sprintf(buf, ", Can't open %s (new)", name);
       G_fatal_error(buf);
    }
    rast = G_allocate_raster_buf(map_type);

    fprintf(stdout, "Writing new data for [%s]... ", name);
    fflush(stdout);
    /* the null file is written automatically */
    for(row = 0; row < cellhd.rows; row++)
    {
       G_percent (row, cellhd.rows, 1);

       if(G_get_raster_row_nomask(old, rast, row, map_type) < 0)
       {
	  sprintf(buf, "Can't read row %d\n", row);
	  G_warning(buf);
	  break;
       }

       mask_raster_array(rast, cellhd.cols, change_null, map_type);

       if(G_put_raster_row(new, rast, map_type) < 0)
       {
	  sprintf(buf, "Can't write row %d\n", row);
	  G_warning(buf);
	  break;
       }
    }
    G_percent (row, cellhd.rows, 1);
    G_free (rast);
    G_close_cell (old);
    if (row < cellhd.rows)
    {
	G_unopen_cell (new);
	return 1;
    }
    G_close_cell (new);
    return 0;
    fprintf(stdout,"\n");
    fflush(stdout);
}
