#include "geo.h"
#include "local_proto.h"

int getargs (int argc, char *argv[], struct GEO *geo, char **infile, char **outfile)
{
    double lat,lon;

	struct GModule *module;
    struct
    {
	struct Option *input, *output,
		      *dim, *res,
		      *corner,
		      *spheroid,
		      *bpc;
    } parm;
    struct
    {
	struct Flag *s;
    } flag;
    char corner_description[256];
    char *G_lat_format_string(), *G_lon_format_string();

	module = G_define_module();
	module->description =
		"Converts raster data referenced using latitude and longitude "
		"coordinates to a UTM-referenced map layer in GRASS raster format.";

    parm.input = G_define_option();
    parm.input->key = "input";
    parm.input->description = "input file";
    parm.input->type = TYPE_STRING;
    parm.input->required = YES;
#define INPUT parm.input->answer

    parm.output = G_define_option();
    parm.output->key = "output";
    parm.output->description = "output raster file";
    parm.output->type = TYPE_STRING;
    parm.output->required = YES;
    parm.output->gisprompt = "new,cell,raster";
#define OUTPUT parm.output->answer

    parm.bpc = G_define_option();
    parm.bpc->key = "bpc";
    parm.bpc->description = "number of bytes per cell";
    parm.bpc->type = TYPE_INTEGER;
    parm.bpc->required = YES;
#define BPC parm.bpc->answer

    parm.corner = G_define_option();
    parm.corner->key = "corner";
    parm.corner->key_desc = "corner,lat,lon";
    parm.corner->description = corner_description;
    sprintf (corner_description,
	"one corner latitude and longitude of the input\nformat: {nw|ne|sw|se},%s,%s",
	G_lat_format_string(), G_lon_format_string());;
    parm.corner->type = TYPE_STRING;
    parm.corner->required = YES;
#define CORNER parm.corner->answers[0]
#define LAT parm.corner->answers[1]
#define LON parm.corner->answers[2]

    parm.dim = G_define_option();
    parm.dim->key = "dimension";
    parm.dim->key_desc = "rows,cols";
    parm.dim->description = "number of rows and columns in the input file";
    parm.dim->type = TYPE_INTEGER;
    parm.dim->required = YES;
#define ROWS parm.dim->answers[0]
#define COLS parm.dim->answers[1]

    parm.res = G_define_option();
    parm.res->key = "res";
    parm.res->key_desc = "latres,lonres";
    parm.res->description = "resolution of the input (in arc seconds)";
    parm.res->type = TYPE_STRING;
    parm.res->required = YES;
#define LATRES parm.res->answers[0]
#define LONRES parm.res->answers[1]

    parm.spheroid = G_define_option();
    parm.spheroid->key = "spheroid";
    parm.spheroid->description = "spheroid";
    parm.spheroid->type = TYPE_STRING;
    parm.spheroid->required = YES;
    parm.spheroid->options = spheroid_list();
#define SPHEROID parm.spheroid->answer

    flag.s = G_define_flag();
    flag.s->key = 's';
    flag.s->description = "Signed data (high bit means negative value)";

    if (G_parser(argc,argv))
	usage(0); /* this routine exits */

/* signed data? */
    geo->sflag = flag.s->answer;

    *infile = INPUT;
    *outfile = OUTPUT;

/* corner */
    if (!scan_lat (LAT, &lat))
    {
	fprintf(stderr, "ERROR: <%s>: invalid latitude\n", LAT);
	usage(1);
    }
    if (!scan_lon (LON, &lon))
    {
	fprintf(stderr, "ERROR: <%s>: invalid longitude\n", LON);
	usage(1);
    }

/* spheroid */
    if(!G_get_ellipsoid_by_name (SPHEROID, &geo->a, &geo->e))
    {
	fprintf(stderr, "ERROR: <%s>: unknown spheroid\n", SPHEROID);
	usage(1);
    }

/* rows and cols */
    if(sscanf (ROWS, "%d", &geo->nrows) != 1 || geo->nrows < 1)
    {
	fprintf(stderr, "ERROR: <%s>: illegal number of rows\n", ROWS);
	usage(1);
    }
    if(sscanf (COLS, "%d", &geo->ncols) != 1 || geo->ncols < 1)
    {
	fprintf(stderr, "ERROR: <%s>: illegal number of columns\n", COLS);
	usage(1);
    }

/* resolution */
    if (sscanf(LATRES, "%lf", &geo->lat_res) != 1 || geo->lat_res <= 0.0)
    {
	fprintf(stderr, "ERROR: <%s>: illegal latitude resolution\n", LATRES);
	usage(1);
    }
    if (sscanf(LONRES, "%lf", &geo->lon_res) != 1 || geo->lon_res <= 0.0)
    {
	fprintf(stderr, "ERROR: <%s>: illegal longitude resolution\n", LONRES);
	usage(1);
    }

/* corner */
/* note: the lat,lon are center of cell.
 * They need to be adjust to edge of cell
 */
    if(strcmp(CORNER,"nw") == 0)
    {
	geo->lat = lat + 0.5;
	geo->lon = lon + 0.5;
    }
    else if(strcmp(CORNER,"ne") == 0)
    {
	geo->lat = lat + 0.5;
	geo->lon = lon + geo->ncols * geo->lon_res - 0.5;
    }
    else if(strcmp(CORNER,"se") == 0)
    {
	geo->lat = lat + geo->nrows * geo->lat_res - 0.5;
	geo->lon = lon + geo->ncols * geo->lon_res - 0.5;
    }
    else if(strcmp(CORNER,"sw") == 0)
    {
	geo->lat = lat + geo->nrows * geo->lat_res - 0.5;
	geo->lon = lon + 0.5;
    }
    else
    {
	fprintf(stderr, "ERROR: <%s>: invalid corner specification\n", CORNER);
	usage(1);
    }

/* bpc */
    if (sscanf (BPC, "%d", &geo->bpc) != 1 || geo->bpc < 1)
    {
	fprintf(stderr, "ERROR: <%s>: illegal number of bytes per cell\n", BPC);
	usage(1);
    }

    return 1;
}

int 
usage (int full)
{
	if (full) G_usage();
	exit(1);
}
