#define MAIN
#include "ps_info.h"
#include "map_info.h"
#include "vector.h"
#include "labels.h"
#include "header.h"
#include "colortable.h"
#include "sites.h"

FILE *tracefd;
FILE *inputfd;

#define KEY(x) (strcmp(key,x)==0)

static char *help[]=
{
    "rast       rasterfile            setcolor   cat(s) color",
    "sites      sitefile              defpat     name",
    "vect       vectorfile            setpat     cat pattern | builtin | all",
    "labels     labelfile             text       east north text",
    "region     regionfile            line       east north east north",
    "grid       spacing               point      east north",
    "outline",
    "colortable [y|n]",
    "comments   [unix-file]",
    "read       unix-file",
    "verbose    [0|1|2]",
    "scale      1:#|# inches|# panels|1 inch = # miles",
    "colormode  best | approx",
    ""
};
int verbose;

#include <signal.h>
main(argc,argv) char *argv[];
{
    struct Cell_head window;
    char buf[1024];
    char name[100], mapset[50];
    int i;
    int iflag;
    int can_reset_scale ;
    int exit();
    struct Option *map_scale;
    struct Option *input_file;
    struct Option *output_file;
    static char *def_font = "Helvetica";

    /**************** begin ******************************/
    read_cfg();

    signal (SIGINT, exit);
    signal (SIGTERM, exit);

    setbuf (stderr, NULL);
    G_gisinit(argv[0]) ;

    input_file = G_define_option();
    input_file->key = "input";
    input_file->type = TYPE_STRING;
    input_file->description = "file containing mapping instructions (or use input=- to enter from keyboard)";
    input_file->required = NO;

    map_scale = G_define_option();
    map_scale->key = "scale";
    map_scale->key_desc = "mapscale";
    map_scale->type = TYPE_STRING;
    map_scale->description = "scale of the output map, e.g. 1:25000 (default: 1panel)";

    output_file = G_define_option();
    output_file->key = "output";
    output_file->type = TYPE_STRING;
    output_file->description = "PostScript output file";
    output_file->required = YES;


    if (!isatty(0)) G_disable_interactive();
    if (G_parser(argc, argv)) usage(0);

    BLACK = get_color_number("black");
    WHITE = get_color_number("white");
    GREY  = get_color_number("grey");

    /* initialize */
    m_info.x = m_info.y  = -1.0;
    vector.x = vector.y  = -1.0;
    ct.x     = ct.y      = -1.0;
    ct.width = -1.0;
    m_info.color = BLACK;
    m_info.font = G_store(def_font);
    vector.font = G_store(def_font);
    hdr.font    = G_store(def_font);
    ct.font     = G_store(def_font);
    m_info.size = 10;
    vector.size = 10;
    hdr.size    = 10;
    ct.size     = 10;
    ct.cols     = 1;
    tracefd = NULL;
    inputfd = stdin;
    iflag = 0;
    verbose = 2;
    labels.count = 0;
    labels.other = NULL;
    vector.count = 0;
    site.count = 0;
    can_reset_scale = 1;
    hdr.fp = NULL;
    PS.do_header = 1;
    PS.min_y = 72.0 * (PS.page_height - PS.top_marg);
    PS.set_y = 100.0 * PS.min_y;
    PS.startpanel = 0;
    PS.endpanel = 0;
    PS.cell_fd = -1;
    PS.do_outline = 0;
    PS.do_colortable = 0;
    PS.grid = 0;
    PS.scaletext[0] = 0;
    PS.celltitle[0] = 0;

    /* arguments */
    if (input_file->answer && strcmp (input_file->answer, "-"))
    {
	if (NULL == freopen(input_file->answer, "r", stdin))
	{
	    fprintf(stderr, "%s - ", G_program_name());
	    perror(input_file->answer);
	    exit(1);
	}
    }
    if (map_scale->answer)
    {
	can_reset_scale = isatty(0);
	if (check_scale(map_scale->answer))
	    strcpy(PS.scaletext, map_scale->answer);
	else error(map_scale->answer, "", "illegal scale request");
    }
    if (output_file->answer)
    {
	if ((PS.fp = fopen(output_file->answer, "w")) == NULL)
	{
	    fprintf(stderr, "%s - ", G_program_name());
	    perror(output_file->answer);
	    exit(1);
	}
    }
    else usage(1);

    /* get current mapset */
    PS.cell_mapset = G_mapset();

    /* set current window */
    G_get_set_window(&PS.w);
    if (G_set_window(&PS.w) == -1)
	G_fatal_error("Current window not settable");

    while (1)
    {
	char *key;
	char *data;

	if (!input(1, buf, help))
	{
	    if (!iflag) break;
	    iflag = 0;
	    continue;
	}
	if (!key_data(buf, &key, &data))
	    continue ;

/*
	if (KEY("session"))
	{
	    print_session(stdout);
	    reject();
	    continue;
	}
*/
/*

	if (KEY("read"))
	{
	    if (inputfd != stdin) fclose(inputfd);

	    if (sscanf(data, "%s", name) != 1)
	    {
		error(key, data, "no file specified");
		inputfd = stdin;
	    }
	    else if ((inputfd = fopen(name, "r")) == NULL)
	    {
		error(key, data, "unable to open");
		inputfd = stdin;
	    }
	    else iflag = 1;
	    continue;
	}
*/

	if (KEY("verbose"))
	{
	    if (sscanf(data, "%d", &verbose) != 1) verbose = 2;
	    continue;
	}

	if (KEY("maploc"))
	{
	    int n;
	    double x, y, w, h;

	    n = sscanf(data, "%lf %lf %lf %lf", &x, &y, &w, &h);
	    if (n == 2 || n == 4)
	    {
		if (y < PS.top_marg)  y = PS.top_marg;
		if (x < PS.left_marg) x = PS.left_marg;
    		PS.map_x_orig = x;
    		PS.map_y_orig = PS.page_height - y;
		PS.set_y = 72.0 * PS.map_y_orig;
		if (n == 4)
		{
		    if (h > PS.page_height - PS.bot_marg   - y)
		        h = PS.page_height - PS.bot_marg   - y;
		    if (w > PS.page_width  - PS.right_marg - x)
		        w = PS.page_width  - PS.right_marg - x;
    		    PS.map_width = w;
    		    PS.map_height = h;
		}
		else
		{
		    PS.map_width  = PS.page_width  - PS.right_marg - x;
		    PS.map_height = PS.page_height - PS.bot_marg   - y;
		}
	    }
	    else
	    {
		error(key, data, "illegal maploc request");
		gobble_input();
	    }
	    continue;
	}

/*
	if (KEY("defpat"))
	{
	    if (sscanf(data, "%s %1s", name, mapset) == 1)
		input_pattern(name);
	    else
	    {
		error(key, data, "illegal defpat request");
		gobble_input();
	    }
	    continue;
	}
*/
/*

	if (KEY("setpat"))
	{
	    int cat ;

	    if (parms.cellfd < 0)
		error(key, data, "no raster file selected yet");
	    else if (sscanf(data, "%s", name) == 1 && 
		strcmp(name, "builtin") == 0) builtin_patterns();
	    else if (sscanf(data, "%s", name) == 1 && 
		strcmp(name, "all") == 0)
	    {
		if (!any_patterns()) error(key, data, "no patterns defined");
		else set_all_patterns();
	    }
	    else if (sscanf(data, "%d %s", &cat, name) != 2)
		error(key, data, "illegal setpat request");
	    else if (!set_pattern (cat, name))
		error("pattern", name, "not found");
	    continue;
	}
*/
/*

	if (KEY("setcolor"))
	{
	    int r, g, b;
	    int color;
	    int count;
	    int *list;
	    int min, max;
	    char colorbuf[100];
	    char catsbuf[100];

	    if (parms.cellfd < 0)
	    {
		error(key, data, "no raster file selected yet");
		continue;
	    }
	    if (sscanf(data, "%s %[^\n]", catsbuf, colorbuf) == 2)
	    {
		if (!scan_color(colorbuf, &color, &r, &g, &b))
		{
		    error(key, data, "illegal color");
		    continue;
		}
		if ((count = parse_number_list(catsbuf, &list)) < 0)
		{
		    error(key, data, "illegal category list");
		    continue;
		}
		for (i = 0; i < count; i += 2)
		{
		    min = list[i];
		    max = list[i + 1];
		    G_add_color_rule((CELL)min, r, g, b, (CELL)max, r, g, b, 
			&parms.pcolr);
		}
		free(list);
	    }
	    continue;
	}
*/

	if (KEY("colortable"))
	{
	    PS.do_colortable = 0;
	    if (PS.cell_fd < 0)
		error(key, data, "no raster file selected yet");
	    else
		PS.do_colortable = yesno(key, data);
	    if (PS.do_colortable) ctablfile();
	    continue;
	}
/*
	if (KEY("text"))
	{
	    double e, n;
	    char east[50], north[50];
	    char text[200];

	    if (sscanf(data, "%s %s %[^\n]", east, north, text) == 3
	        && (scan_easting(east, &e) && scan_northing(north, &n)))
		    record_label(east, north, text);
	    else
	    {
		gobble_input();
		error(key, data, "illegal text request");
	    }
	    continue;
	}
*/
/*

	if (KEY("point"))
	{
	    double e, n;
	    char east[50], north[50];

	    if (sscanf(data, "%s %s", east, north) == 2
	        && (scan_easting(east, &e) && scan_northing(north, &n)))
		    record_point(e, n);
	    else
	    {
		gobble_input();
		error(key, data, "illegal point request");
	    }
	    continue;
	}
*/
/*

	if (KEY("line"))
	{
	    char east1[50], north1[50];
	    char east2[50], north2[50];
	    double e1, n1, e2, n2;

	    if (sscanf(data, "%s %s %s %s", east1, north1, east2, north2) == 4
	        && (scan_easting(east1, &e1)  && scan_easting(east2, &e2)
	        && scan_northing(north1, &n1) && scan_northing(north2, &n2)))
		    record_line(e1, n1, e2, n2);
	    else
	    {
		gobble_input();
		error(key, data, "illegal line request");
	    }
	    continue;
	}
*/
/*

	if (KEY("comments"))
	{
	    switch (sscanf(data, "%s %s", name, mapset))
	    {
	        case 1: commentfile(name); break;
	        case 2: error(key, data, "illegal comments request"); break;
	        default: commentfile(""); break;
	    }
	    continue;
	}
*/
/*

	if (KEY("startpanel"))
	{
	    if (sscanf(data, "%d", &PS.startpanel) != 1 || PS.startpanel <= 0)
	    {
		PS.startpanel = 0;
		error(key, data, "illegal startpanel");
	    }
	    continue;
	}
*/
/*

	if (KEY("endpanel"))
	{
	    if (sscanf(data, "%d", &PS.endpanel) != 1 || PS.endpanel <= 0)
	    {
		PS.endpanel = 0;
		error(key, data, "illegal endpanel");
	    }
	    continue;
	}
*/

	if (KEY("scale"))
	{
	    if (!can_reset_scale) continue;
	    if (check_scale(data)) strcpy(PS.scaletext, data);
	    else
	    {
		PS.scaletext[0] = 0;
		error(key, data, "illegal scale request");
	    }
	    continue;
	}

	if (KEY("labels"))
	{
	    if (scan_gis("paint/labels", "label", key, data, name, mapset, 1))
		labelfile(name, mapset);
	    continue;
	}
/*

	if (KEY("sites"))
	{
	    if (scan_gis("site_lists", "site list", key, data, name, mapset, 1))
		sitefile(name, mapset);
	    continue;
	}
*/

	if (KEY("header"))
	{
	    hdrfile();
	    continue;
	}

	if (KEY("mapinfo"))
	{
	    infofile();
	    continue;
	}

	if (KEY("vlegend"))
	{
	    vlegfile();
	    continue;
	}

	if (KEY("outline"))
	{
	    if (PS.cell_fd < 0)
	    {
		error(key, data, "no raster file selected yet");
		gobble_input();
	    }
	    else outlinefile();
	    continue;
	}

	if (KEY("cell") || KEY("rast") || KEY("raster"))
	{
	    if (scan_gis("cell", "raster", key, data, name, mapset, 0))
		cellfile(name, mapset);
	    continue;
	}

	if (KEY("vector") || KEY("vect"))
	{
	    if (scan_gis("dig", "vector", key, data, name, mapset, 1))
		vectfile(name, mapset);
	    continue;
	}
 
/*
	if (KEY ("window") || KEY("region"))
	{
	    if (scan_gis("windows", "region definition", key, data, name,
		mapset, 1)) windfile(name, mapset);
	    continue;
	}
*/

	if (KEY("grid"))
	{
	    PS.grid = -1;
	    PS.grid_numbers = 0;
	    sscanf(data, "%d", &PS.grid);
	    if (PS.grid < 0)
	    {
		PS.grid = 0;
		error(key, data, "illegal grid spacing");
		gobble_input();
	    }
/*
	    else
		getgrid();
*/
	    continue;
	}

	if (*key)
	    error(key,"","illegal request");
    }

    /*unlink_pattern_file();*/

    /* write the PostScript output file */
    ps_map();
    if (verbose > 1)
    {
        printf("PS-PAINT: PostScript file \"%s\" successfully written.\n",
    		output_file->answer);
        fflush(stdout);
    }
    exit(0);
}


usage(full)
{
    if (full) G_usage();
    exit(1);
}
