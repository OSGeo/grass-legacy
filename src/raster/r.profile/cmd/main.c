#include "gis.h"
#include "local_proto.h"

int main (int argc, char *argv[])
{
    double e1,e2,n1,n2;
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
    struct Cell_head window;
    int fd;
    CELL *cell;

    G_gisinit (argv[0]);

	module = G_define_module();
    module->description =
		"Outputs the raster map layer values "
		"lying on user-defined line(s).";

    G_get_window(&window);
    projection = G_projection();

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
    parms.line->key_desc = "east,north,east,north";
    parms.line->type = TYPE_STRING;
    parms.line->description = "Profile coordinates";
    parms.line->required = YES;
    parms.line->multiple = YES;

    parms.width = G_define_option();
    parms.width->key = "width";
    parms.width->type = TYPE_INTEGER;
    parms.width->description = "Profile width, in cells (odd number)";
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
    init_profiles(n, parms.result->answer);

    name = parms.map->answer;
    mapset = G_find_cell(name,"");
    if (mapset == NULL)
    {
	fprintf (stderr, "%s: <%s> raster map not found\n",
		G_program_name(), name);
	exit(1);
    }
    fd = G_open_cell_old (name, mapset);
    if (fd < 0)
	exit(1);
    cell = G_allocate_cell_buf();

    err = 0;
    for (n=0; parms.line->answers[n]; n+=4)
	err += parse_line(parms.line->key, parms.line->answers+n,
		&e1, &n1, &e2, &n2, projection);
    if (err)
    {
	G_usage();
	exit(1);
    }
    for (n=0; parms.line->answers[n]; n+=4)
    {
	parse_line(parms.line->key, parms.line->answers+n,
		&e1, &n1, &e2, &n2, projection);
	process_line (fd, cell, e1, n1, e2, n2, &window);
    }

    exit(0);
}
