#include "gis.h"

int main(int argc, char *argv[])
{
    char *mapset, *name;
    double east, north;
    char ebuf[128], nbuf[128];
    char *desc;
    Site *site;
    char *fs;
    int full, all, strip, n=0, s=0, d=0;
    RASTER_MAP_TYPE c=-1;
    char msg[200];
    FILE *fd;
    struct Cell_head window;
    struct GModule *module;
    struct
    {
	struct Flag *full, *all, *strip;
    } flag;
    struct
    {
	struct Option *input, *fs;
    } parm;
    int G_trim_decimal ();

    G_gisinit (argv[0]);

    module = G_define_module();
    module->description =        
                    "Converts a GRASS site list file into an ASCII listing of "
                    "site locations and their descriptions.";
                    
    parm.input = G_define_option();
    parm.input->key = "sites";
    parm.input->type = TYPE_STRING;
    parm.input->required = YES;
    parm.input->description = "name of a sites file to be output";
    parm.input->gisprompt = "old,site_lists,sites";

    parm.fs = G_define_option();
    parm.fs->key = "fs";
    parm.fs->key_desc = "character|space|tab";
    parm.fs->type = TYPE_STRING;
    parm.fs->required = NO;
    parm.fs->description = "Output field separator";
    parm.fs->answer = "space";

    flag.all = G_define_flag();
    flag.all->key = 'a';
    flag.all->description = "Output all sites (do not limit to current region)";

    flag.full = G_define_flag();
    flag.full->key = 'd';
    flag.full->description = "Include site attributes in the output";

    flag.strip = G_define_flag();
    flag.strip->key = 'i';
    flag.strip->description = "Include site attribute identifiers in the output";

    if (G_parser(argc,argv))
	exit(1);
    full = flag.full->answer;
    all  = flag.all->answer;
    strip  = flag.strip->answer;

    name = parm.input->answer;
    mapset = G_find_file ("site_lists", name, "");
    if (mapset == NULL)
    {
	sprintf (msg, "sites file [%s] not found", name);
	G_fatal_error (msg);
    }

    if (fs = parm.fs->answer)
    {
	if(strcmp (fs, "space") == 0)
	    fs = " ";
	else if(strcmp (fs, "tab") == 0)
	    fs = "\t";
    }
    else
	fs = " ";

    if (!all)
	G_get_window (&window);
    fd = G_fopen_sites_old (name, mapset);
    if (fd == NULL)
    {
	sprintf (msg, "can't open sites file [%s]", name);
	G_fatal_error (msg);
    }

    if (G_site_describe (fd, &n, &c, &s, &d)!=0)
      G_fatal_error("failed to guess format");
    site = G_site_new_struct (c, n, s, d);

    while (G_site_get (fd, site) == 0)
    {
	if (all || G_site_in_region (site, &window))
	{
	    if (!full)
            {
	      G_format_easting (site->east, ebuf, -1);
	      G_format_northing (site->north, nbuf, -1);
	      fprintf (stdout,"%s%s%s", ebuf, fs, nbuf);
              for (n = 0; n < site->dim_alloc; ++n)
              {
                sprintf (nbuf, "%.8f", site->dim[n]);
                G_trim_decimal (nbuf);
                fprintf (stdout,"%s%s", fs, nbuf);
              }
              fprintf (stdout,"\n");
            }
            else
	      fprintf (stdout,"%s\n", G_site_format (site, fs, strip));
	}
    }
    fclose (fd);
    exit(0);
}
