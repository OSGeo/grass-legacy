#include <string.h>
#include <stdlib.h>
#include "gis.h"
#include "local_proto.h"

int main (int argc, char *argv[])
{
    double e1,n1,e2,n2;
    char buf[256];
    char command[2048];

    int n,err;
    int projection;
    char *name, *mapset;

	struct GModule *module;
    struct
    {
	struct Option *map;
	struct Option *line;
	struct Option *width;
	struct Option *result;
    } parms;

    G_gisinit (argv[0]);
    projection = G_projection();

	module = G_define_module();
	module->description =
		"Outputs raster map layer values lying along "
		"user defined transect line(s).";

    parms.map = G_define_option();
    parms.map->key = "map";
    parms.map->type = TYPE_STRING;
    parms.map->description = "Raster map to be queried";
    parms.map->required = YES;
    parms.map->multiple = NO;

    parms.result = G_define_option();
    parms.result->key = "result";
    parms.result->key_desc = "type";
    parms.result->type = TYPE_STRING;
    parms.result->description = "Type of result to be output";
    parms.result->required = NO;
    parms.result->multiple = NO;
    parms.result->options = "raw,median,average";
    parms.result->answer = "raw";

    parms.line = G_define_option();
    parms.line->key = "line";
    parms.line->key_desc = "east,north,azimuth,distance";
    parms.line->type = TYPE_STRING;
    parms.line->description = "Transect definition";
    parms.line->required = YES;
    parms.line->multiple = YES;

    parms.width = G_define_option();
    parms.width->key = "width";
    parms.width->type = TYPE_INTEGER;
    parms.width->description = "Transect width, in cells (odd number)";
    parms.width->answer = "1";

    if (G_parser(argc,argv))
	exit(1);

    sscanf (parms.width->answer, "%d", &n);
    if (n <= 0 || n%2 == 0)
    {
	fprintf(stderr,"<%s=%s> ** illegal value **\n",
	    parms.width->key, parms.width->answer);
	G_usage();
	exit(1);
    }

    name = parms.map->answer;
    mapset = G_find_cell(name,"");
    if (mapset == NULL)
    {
	fprintf (stderr, "%s: <%s> raster map not found\n",
		G_program_name(), name);
	exit(1);
    }
    sprintf (command, "r.profile map='%s' width=%s result=%s line=",
	parms.map->answer, parms.width->answer, parms.result->answer);
    err = 0;
    for (n=0; parms.line->answers[n]; n+=4)
    {
	err += parse_line(parms.line->key, parms.line->answers+n,
		&e1, &n1, &e2, &n2, projection);
	if (!err)
	{
	    if (n)
		strcat (command, ",");
	    G_format_easting(e1,buf,projection);
	    strcat (command,buf);
	    G_format_northing(n1,buf,projection);
	    strcat (command, ",");
	    strcat (command,buf);
	    G_format_easting(e2,buf,projection);
	    strcat (command, ",");
	    strcat (command,buf);
	    G_format_northing(n2,buf,projection);
	    strcat (command, ",");
	    strcat (command,buf);
	}
    }
    if (err)
    {
	G_usage();
	exit(1);
    }
    exit (system(command));
}
