#include "gis.h"
#include "raster.h"
#define MAIN
#include "mask.h"
#include "local_proto.h"

static int parse_catlist ( char **, Mask *);
static int parse_vallist ( char **, d_Mask *);

int main(
    int argc,
    char **argv)
{
    char *mapset ;
    char *name ;
    int overlay;
    int invert, fp;
	struct GModule *module;
    struct Option *map;
    struct Option *catlist;
    struct Option *vallist;
    struct Option *bg;
    struct Flag *flag_o;
    struct Flag *flag_i;

/* Initialize the GIS calls */
    G_gisinit(argv[0]) ;

	module = G_define_module();
	module->description =
		"Displays and overlays raster map layers "
		"in the active display frame on the graphics monitor.";

/* set up command line */
    map              = G_define_option();
    map->key         = "map";
    map->type        = TYPE_STRING;
    map->required    = YES;
    map->gisprompt   = "old,cell,raster" ;
    map->description = "Raster map to be displayed";

    catlist              = G_define_option();
    catlist->key         = "catlist";
    catlist->key_desc    = "cat[-cat]";
    catlist->type        = TYPE_STRING;
    catlist->required    = NO;
    catlist->multiple    = YES;
    catlist->description = "List of categories to be displayed";

    vallist              = G_define_option();
    vallist->key         = "vallist";
    vallist->key_desc    = "val[-val]";
    vallist->type        = TYPE_STRING;
    vallist->required    = NO;
    vallist->multiple    = YES;
    vallist->description = "List of values to be displayed";

    bg              = G_define_option();
    bg->key         = "bg";
    bg->key_desc    = "color";
    bg->type        = TYPE_STRING;
    bg->required    = NO;
    bg->options     = color_list();
    bg->description = "Background color (for null)";

    flag_o = G_define_flag();
    flag_o->key = 'o';
    flag_o->description = "Overlay (non-null values only)";

    flag_i = G_define_flag();
    flag_i->key = 'i';
    flag_i->description = "Invert catlist";

    if (G_parser(argc, argv))
	exit(1);

    name = map->answer;
    overlay = flag_o->answer;
    invert = flag_i->answer;

/* Make sure map is available */
    mapset = G_find_cell2 (name, "") ;
    if (mapset == NULL)
    {
	char buf[256];
        sprintf(buf,"Raster map [%s] not available", name);
        G_fatal_error(buf) ;
    }

    if (R_open_driver() != 0)
	G_fatal_error ("No graphics device selected");

    fp = G_raster_map_is_fp(name, mapset);
    if(catlist->answer)
    {
       if(fp) G_warning("Ignoring catlist: map is floating point");
       else parse_catlist (catlist->answers, &mask);
    }
    if(vallist->answer)
    {
       if(!fp) G_warning("Ignoring vallist: map is integer");
       else parse_vallist (vallist->answers, &d_mask);
    }

    /* use DCELL even if the map is FCELL */

    if(fp)
        display (name, mapset, overlay, bg->answer, DCELL_TYPE, invert) ;
    else
        display (name, mapset, overlay, bg->answer, CELL_TYPE, invert) ;

    R_close_driver();

    exit(0);
}

static int parse_catlist ( char **catlist, Mask *mask)
{
    char buf[1024];
    char x[2];
    FILE *fd;

    init_mask_rules (mask);
    if (catlist == NULL) return 0;

    for ( ; *catlist; catlist++)
    {
	if (*catlist[0] == '/')
	{
	    fd = fopen (*catlist, "r");
	    if (fd == NULL)
	    {
		perror (*catlist);
		G_usage();
		exit(1);
	    }
	    while (fgets (buf, sizeof buf, fd))
	    {
		if (sscanf (buf, "%1s", x) != 1 || *x == '#')
		    continue;
		parse_mask_rule (buf, mask, *catlist);
	    }
	    fclose(fd);
	}
	else
	    parse_mask_rule (*catlist, mask, (char *)NULL);
    }

    return 0;
}

static int parse_vallist ( char **vallist, d_Mask *d_mask)
{
    char buf[1024];
    char x[2];
    FILE *fd;

    init_d_mask_rules (d_mask);
    if (vallist == NULL) return -1;

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
parse_mask_rule (char *catlist, Mask *mask, char *where)
{
    long a,b;
    char junk[128];

/* #-# */
    if (sscanf (catlist,"%ld-%ld",&a,&b) == 2)
	add_mask_rule (mask, a, b, 0);

/* inf-# */
    else if (sscanf (catlist,"%[^ -\t]-%ld", junk, &a) == 2)
	add_mask_rule (mask, a, a, -1);

/* #-inf */
    else if (sscanf (catlist,"%ld-%[^ \t]", &a, junk) == 2)
	add_mask_rule (mask, a, a, 1);

/* # */
    else if (sscanf (catlist,"%ld",&a) == 1)
	add_mask_rule (mask, a, a, 0);

    else
    {
	if(where)
	    fprintf (stderr, "%s: ", where);
	fprintf (stderr, "%s: illegal category spec\n", catlist);
	G_usage();
	exit(1);
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
	add_d_mask_rule (d_mask, a, b, 0);

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
