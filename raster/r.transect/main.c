#include <string.h>
#include <stdlib.h>
#include "gis.h"
#include "local_proto.h"
#include "glocale.h"

int main (int argc, char *argv[])
{
    double e1,n1,e2,n2;
    char buf[256];
    char command[2048];

    int n,err;
    int projection;
    char *mapset;
    char name[256];

    struct GModule *module;
    struct
    {
	struct Option *map;
	struct Option *line;
	struct Option *null_str;
/*	struct Option *width;
	struct Option *result; */
    } parms;
    struct Flag *coord;
    char coord_str[3];

    G_gisinit (argv[0]);

    module = G_define_module();
    module->description =
	_("Outputs raster map layer values lying along "
	"user defined transect line(s).");

    parms.map = G_define_option();
    parms.map->key = "map";
    parms.map->type = TYPE_STRING;
    parms.map->description = _("Raster map to be queried");
    parms.map->required = YES;
    parms.map->multiple = NO;

/*  parms.result = G_define_option();
    parms.result->key = "result";
    parms.result->key_desc = "type";
    parms.result->type = TYPE_STRING;
    parms.result->description = _("Type of result to be output");
    parms.result->required = NO;
    parms.result->multiple = NO;
    parms.result->options = "raw,median,average";
    parms.result->answer = "raw";
*/
    parms.line = G_define_option();
    parms.line->key = "line";
    parms.line->key_desc = "east,north,azimuth,distance";
    parms.line->type = TYPE_STRING;
    parms.line->description = _("Transect definition");
    parms.line->required = YES;
    parms.line->multiple = YES;

    parms.null_str = G_define_option() ;
    parms.null_str->key        = "null";
    parms.null_str->type       = TYPE_STRING;
    parms.null_str->required   = NO;
    parms.null_str->answer     = "*";
    parms.null_str->description= _("Char string to represent no data cell") ;

/*  parms.width = G_define_option();
    parms.width->key = "width";
    parms.width->type = TYPE_INTEGER;
    parms.width->description = _("Transect width, in cells (odd number)");
    parms.width->answer = "1";
*/

    coord = G_define_flag();
    coord->key = 'g';
    coord->description =
	_("Output easting and northing in first two columns of four column output");

    if (G_parser(argc,argv))
	exit(1);

    projection = G_projection();

/*  sscanf (parms.width->answer, "%d", &n);
    if (n <= 0 || n%2 == 0)
    {
	fprintf(stderr,"<%s=%s> ** illegal value **\n",
	    parms.width->key, parms.width->answer);
	G_usage();
	exit(1);
    }
*/

    strncpy(name, parms.map->answer, 255);
    mapset = G_find_cell(name,"");

    if (mapset == NULL)
    {
	fprintf (stderr, "%s: <%s> raster map not found\n",
		G_program_name(), name);
	exit(1);
    }

    if(coord->answer)  
	strcpy(coord_str, "-g");
    else
	strcpy(coord_str, "");

    sprintf (command, "r.profile %s input='%s' output='-' null='%s' profile=", 
		coord_str, parms.map->answer, parms.null_str->answer);

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
