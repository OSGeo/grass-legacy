#include "gis.h"
#include <stdlib.h>

int main(int argc, char *argv[])
{
    char name[256];
    int uncompressed;
    char buf[1024];
    struct Cell_head cellhd;
    char rname[256], rmapset[256];

    G_gisinit (argv[0]);

    while (G_ask_cell_in_mapset("",name))
    {
	if (G_get_cellhd (name, G_mapset(), &cellhd) < 0)
	{
	    fprintf (stderr, "ERROR: unable to read cell header for %s\n", name);
	    continue;
	}
	if (G_is_reclass (name, G_mapset(), rname, rmapset) > 0)
	{
	    fprintf (stdout,"[%s] is a reclass file of map <%s> in mapset <%s> - can't compress/uncompress it\n", name, rname, rmapset);
	    continue;
	}
	uncompressed = !cellhd.compressed;
	sprintf (buf, "%s is %scompressed. Would you like to %scompress it? ",
		name, uncompressed?"un":"", uncompressed?"":"un");
	if (G_yes(buf, uncompressed?1:0))
	{
	    sprintf (buf, "r.compress %s map='%s'", uncompressed?"":"-u", name);
	    system(buf);
	}
	else
	    fprintf (stdout,"%s will not be %scompressed\n", name, uncompressed?"":"un");
    }
    exit(0);
}
