#include <stdlib.h>
#include <unistd.h>
#include <unistd.h>
#include <string.h>
#include "gis.h"
#define MAIN
#include "sites.h"
#include "dlg.h"
#include "vector.h"
#include "Paintlib.h"
#include "graphics.h"
#include "fullwindow.h"
#include "labels.h"
#include "cats.h"
#include "parms.h"
#include "misc.h"
#include "colormode.h"
#include "clegend.h"
#include "barscale.h"
#include "text.h"
#include "grid.h"
#include "regionline.h"
#include "vectrailer.h"
#include "local_proto.h"


FILE *tracefd;
FILE *inputfd;

#define KEY(x) (strcmp(key,x)==0)

static char *help[]=
{
    "rast       rasterfile            setcolor   val_range(s) color",
    "sites      sitefile              defpat     name",
    "vect       vectorfile            setpat     cat pattern | builtin | all",
    "labels     labelfile             text       east north text",
    "region     regionfile            line       east north east north",
    "grid       spacing               point      east north",
    "legend	east north            barscale   east north",
    "outline",
    "colortable [y|n]",
	"rastertitle [y|n]",
	"vectorinfo [y|n",
	"trailer	[y|n]",
    "comments   [unix-file]",
    "read       unix-file",
    "verbose    [0|1|2]",
    "scale      1:#|# inches|# centimeters|# panels|1 inch = # miles",
    "colormode  best | approx",
    ""
};

#include <signal.h>
int main(int argc,char *argv[])
{
	struct GModule *module;
    struct Cell_head window;
    char buf[1024];
    char name[100], mapset[50];
    int i;
    int iflag;
    int can_reset_scale ;
    int can_reset_colormode ;
    struct Option *map_scale;
    struct Option *input_file;

/**************** begin ******************************/

    signal (SIGINT, exit);
    signal (SIGTERM, exit);

    setbuf (stderr, NULL);
    G_gisinit(argv[0]) ;

	module = G_define_module();
	module->description =
		"Color map output utility.";

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

    if (!isatty(0))
	G_disable_interactive();
    if (G_parser(argc,argv))
	usage(0);

/* open the printer */
    Pconnect();
    Popen();
    ncolors = Pncolors();
    BLACK = Pcolornum (0.0, 0.0, 0.0);
    WHITE = Pcolornum (1.0, 1.0, 1.0);
    GREY  = Pcolornum (0.75, 0.75, 0.75);
    set_text_scale (Ptextscale());

/* initialize */
    tracefd = NULL;
    inputfd = stdin;
    iflag = 0;
    verbose = 2;
    labels.count = 0;
    labels.other = NULL;
	catsfile	= NULL;
    vector.count = 0;
    site.count = 0;
    parms.startpanel = 0;
    parms.endpanel = 0;
    parms.cellfd = -1;
    parms.outlinefd = -1;
    parms.with_colortable = 0;
    parms.grid = 0;
    strcpy (grid.gridon, "all");
    strcpy (parms.scaletext, "1 panel");
    can_reset_scale = 1;
    can_reset_colormode = 1;
    parms.cellname = NULL;
    parms.cellmapset = NULL;
    parms.commentfile = NULL;
    parms.plfile = NULL;
    parms.pattern = NULL;
    parms.need_stats = 0;
    graphics.linestyle.table = NULL;

    set_line_style_solid();

/* arguments */
    if (input_file->answer && strcmp (input_file->answer,"-"))
    {
	if(NULL==freopen (input_file->answer, "r", stdin))
	{
	    fprintf (stderr, "%s - ", G_program_name());
	    perror (input_file->answer);
	    exit(1);
	}
    }
    if (map_scale->answer)
    {
	can_reset_scale = isatty(0);
	if (check_scale (map_scale->answer))
	    strcpy (parms.scaletext, map_scale->answer);
	else
	    error (map_scale->answer,"","illegal scale request");
    }

/* get the window */
    G_get_window (&window);
    G_get_window (&fullwindow);

    while (1)
    {
	char *key;
	char *data;

	if (!input (1, buf, help))
	{
	    if (!iflag) break;
	    iflag = 0;
	    continue;
	}

	if(!key_data (buf, &key, &data))
	    continue ;


	if (KEY("session"))
	{
	    print_session(stdout);
	    reject();
	    continue;
	}

	if (KEY("read"))
	{
	    if (inputfd != stdin)
		fclose (inputfd);

	    if (sscanf (data, "%s", name) != 1)
	    {
		error (key,data,"no file specified");
		inputfd = stdin;
	    }
	    else if ((inputfd = fopen (name, "r")) == NULL)
	    {
		error (key,data,"unable to open");
		inputfd = stdin;
	    }
	    else
		iflag = 1;
	    continue;
	}

	if (KEY("verbose"))
	{
	    if (sscanf (data,"%d",&verbose) != 1)
		verbose = 2;
	    continue;
	}

	if (KEY("defpat"))
	{
	    if (sscanf (data, "%s %1s", name, mapset) == 1)
		input_pattern (name);
	    else
	    {
		error (key,data,"illegal defpat request");
		gobble_input();
	    }
	    continue;
	}

	if (KEY("setpat"))
	{
	    int cat=0, cat1=0, cat2=0 ;
		char tmp[1024];
		char *scat1, *scat2;

	    if (parms.cellfd < 0)
		error (key,data,"no raster file selected yet");
	    else if (sscanf (data,"%s",name) == 1 && strcmp (name, "builtin") == 0)
		builtin_patterns();
	    else if (sscanf (data,"%s",name) == 1 && strcmp (name, "all") == 0)
	    {
		if (!any_patterns())
		    error (key,data,"no patterns defined");
		else
		    set_all_patterns();
	    }
	    else if (sscanf (data,"%s %s", tmp, name) != 2)
		error (key,data,"illegal setpat request");
	    else { 
		int pat;
		cat1	= 0;
		cat2	= 0;
		pat = getpat(tmp, &scat1, &scat2);
		if (*scat2)
		{
		cat1	= atoi(scat1);
		if (pat==1)
		cat2	= atoi(scat2);
		else 
		cat2	= 0;
		}
		else { 
			cat1 = atoi(scat1);
			cat2 = cat1;
			}

		for (cat=cat1; cat<=cat2; cat++) {
		if (!set_pattern (cat, name))
		error ("pattern",name,"not found");
		}
		}
		
		if (cat2 ==0) {
		int pat;
		while (*scat2){
		if (!set_pattern (cat, name))
		error ("pattern",name,"not found");
		strcpy(tmp, scat2);
		pat = getpat(tmp, &scat1, &scat2);
		cat = atoi(scat1) ;
		}
		if (!set_pattern (cat, name))
		error ("pattern",name,"not found");
		}
	    continue;
	}

	if (KEY("setcolor"))
	{
	    int r,g,b;
	    int color;
	    int count;
	    DCELL *val_list;
	    DCELL dmin,dmax;
	    char colorbuf[100];
	    char catsbuf[100];

	    if (parms.cellfd < 0)
	    {
		error (key,data,"no raster file selected yet");
		continue;
	    }
	    if (sscanf (data, "%s %[^\n]", catsbuf, colorbuf) == 2)
	    {
		if (!scan_color (colorbuf, &color, &r,&g,&b))
		{
		    error (key,data,"illegal color");
		    continue;
		}
		if(strncmp(catsbuf, "null", 4)== 0)
		{
		    G_set_null_value_color(r,g,b,&parms.pcolr);
		    continue;
                }
		if(strncmp(catsbuf, "default", 7)== 0)
		{
		    G_set_default_color(r,g,b,&parms.pcolr);
		    continue;
                }
		if ((count = parse_val_list (catsbuf, &val_list)) < 0)
		{
		    error (key,data,"illegal value list");
		    continue;
		}
		for (i = 0; i < count; i += 2)
		{
		    dmin = val_list[i];
		    dmax = val_list[i+1];
		    G_add_d_raster_color_rule (&dmin, r,g,b, &dmax,r,g,b, &parms.pcolr);
		}
		free (val_list);
	    }
	    continue;
	}

	if (KEY("legend"))
	{
	    double e,n;
	    char east[50], north[50];
	    parms.need_stats = 1;
	    if  ( (parms.cellfd < 0) && (vector.count <= 0) && 
			  ( site.count <= 0) )
	    {
		error (key,data,"neither a raster file or a vector file or \n a site file has been selected yet");
		continue;
	    }

	    if (sscanf (data,"%s %s",east,north) == 2 
	    && (scan_easting (east, &e) && scan_northing(north, &n))) {
		record_legendtable(east,north);
		}
	    else
	    {
		gobble_input();
		error (key,data,"illegal legend request");
	    }
	    continue;
	}


	if (KEY("barscale"))
	{
	    double e,n;
	    char east[50], north[50];

	    if (sscanf (data,"%s %s",east,north) == 2 
	    && (scan_easting (east, &e) && scan_northing(north, &n))) {
		record_barscale(east,north);
		}
	    else
	    {
		gobble_input();
		error (key,data,"illegal legend request");
	    }
	    continue;
	}




	if (KEY("colortable"))
	{
	    parms.need_stats = 1;
	    parms.with_colortable = 0;
	    if (parms.cellfd < 0)
		error (key,data,"no raster file selected yet");
	    else 
		parms.with_colortable = yesno (key,data);
	    continue;
	}

	if (KEY("rastertitle"))
	{
	    with_rastertitle = 0;
	    if (parms.cellfd < 0)
		error (key,data,"no raster file selected yet");
	    else 
		with_rastertitle = yesno (key,data);
	    continue;
	}



	if (KEY("vectorinfo"))
	{
		with_vectinfo	= 0;
		if (vector.count <= 0)
		error (key,data, "no vector file selected yet");
		else
		with_vectinfo	= yesno(key,data);
		continue;

	}

	if (KEY("trailer"))
	{
		with_trailer	= 0;
		with_trailer	= yesno(key, data);;
		continue;
	}


	if (KEY("text"))
	{
	    double e,n;
	    char east[50], north[50];
	    char text[200];

	    if (sscanf (data,"%s %s %[^\n]",east,north,text) == 3
	    && (scan_easting (east, &e) && scan_northing(north, &n))) {
		record_label (east,north,text);
		}
	    else
	    {
		gobble_input();
		error (key,data,"illegal text request");
	    }
	    continue;
	}

	if (KEY("point"))
	{
	    double e,n;
	    char east[50], north[50];

	    if (sscanf (data,"%s %s", east, north) == 2
	    && (scan_easting (east, &e) && scan_northing(north, &n)))
		record_point (e, n);
	    else
	    {
		gobble_input();
		error (key,data,"illegal point request");
	    }
	    continue;
	}

	if (KEY("line"))
	{
	    char east1[50], north1[50];
	    char east2[50], north2[50];
	    double e1, n1, e2, n2;

	    if (sscanf (data,"%s %s %s %s", east1, north1, east2, north2) == 4
	    && (scan_easting  (east1, &e1)  && scan_easting  (east2,&e2)
	    &&  scan_northing (north1, &n1) && scan_northing (north2,&n2)))
		/*
		record_line (e1,n1,e2,n2);
		*/
		linefile(e1,n1,e2,n2);
	    else
	    {
		gobble_input();
		error (key,data,"illegal line request");
	    }
	    continue;
	}

	if (KEY("comments"))
	{
	    switch (sscanf (data, "%s %s", name, mapset))
	    {
	    case 1: commentfile(name); break;
	    case 2: error (key,data,"illegal comments request"); break;
	    default: commentfile(""); break;
	    }
	    continue;
	}

	if (KEY("startpanel"))
	{
	    if (sscanf (data, "%d", &parms.startpanel) != 1 || parms.startpanel <= 0)
	    {
		parms.startpanel = 0;
		error (key,data,"illegal startpanel");
	    }
	    continue;
	}

	if (KEY("endpanel"))
	{
	    if (sscanf (data, "%d", &parms.endpanel) != 1 || parms.endpanel <= 0)
	    {
		parms.endpanel = 0;
		error (key,data,"illegal endpanel");
	    }
	    continue;
	}

	if (KEY ("scale"))
	{
	    if (!can_reset_scale) continue;
	    if (check_scale (data)) {
		double u1;
		char unit1[30];
		char dummy[2];
    if (sscanf (data, "%lf %s %1s", &u1, unit1, dummy) == 2 &&
	*dummy == 0)
		{
		if (strncmp (unit1, "cent", 4) == 0)
		{
			u1 = (u1/100) * 39.37 ;
		
		sprintf (data, "%f inches", u1);
		}
		}

		strcpy (parms.scaletext, data);
		}
	    else
	    {
		strcpy (parms.scaletext, "1 panel");
		error (key,data,"illegal scale request");
	    }
	    continue;
	}

	if (KEY ("colormode"))
	{
	    if (!can_reset_colormode) continue;
	    if (!set_colormode (data))
		error (key,data,"illegal colormode request");
	    continue;
	}

	if (KEY ("labels"))
	{
	    if (scan_gis ("paint/labels","label",key,data,name,mapset,1))
		labelfile (name, mapset);
	    continue;
	}

	if (KEY ("sites"))
	{
	    if (scan_gis ("site_lists","site list",key,data,name,mapset,1))
		sitefile (name, mapset);
	    continue;
	}

	if (KEY ("outline"))
	{
	    if (parms.cellfd < 0)
	    {
		error (key,data,"no raster file selected yet");
		gobble_input();
	    }
	    else
	    {
		parms.outlinefd = parms.cellfd;
		outlinefile();
	    }
	    continue;
	}

	if (KEY ("cell") || KEY("rast") || KEY("raster"))
	{
	    if (scan_gis ("cell","raster",key,data,name,mapset,0))
		cellfile (name, mapset);
	    continue;
	}

	if (KEY ("vector") || KEY ("vect"))
	{
	    if (scan_gis ("dig","vector",key,data,name,mapset,1))
		vectfile (name, mapset);
	    continue;
	}


	if (KEY ("window") || KEY("region"))
	{
	    if (scan_gis ("windows","region definition",key,data,name,mapset,1))
		/*
		windfile (name, mapset);
		*/
		regionfile (name, mapset);
	    continue;
	}

	if (KEY("grid"))
	{
	    parms.grid = -1;
	    parms.grid_numbers = 0;
	    sscanf (data, "%d", &parms.grid);
	    if (parms.grid < 0)
	    {
		parms.grid = 0;
		error (key,data,"illegal grid spacing");
		gobble_input();
	    }
	    else
		getgrid();
	    continue;
	}

	if (*key)
	    error(key,"","illegal request");
    }

    unlink_pattern_file();

    map (&window);

    Pclose();
    Pdisconnect();
    exit(0);
}

int 
usage (int full)
{
    if (full) G_usage();
    exit(1);
}
