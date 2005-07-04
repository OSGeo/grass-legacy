#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "gis.h"
#include "G3d.h"
#include "local_proto.h"
#include "glocale.h"

#define printline(x) fprintf (out," | %-74.74s |\n",x)
#define divider(x) \
    fprintf (out," %c",x);\
    for (i = 0; i < 76; i++)\
        fprintf(out,"-");\
    fprintf (out,"%c\n",x)

extern char * G_find_grid3 ();

static char *name;

int main (argc, argv)   
    int argc;
    char *argv[];
{
    char *mapset;
    char line[200];
    char tmp1[100], tmp2[100], tmp3[100];
    int i;
    FILE *out;
    G3D_Region cellhd;
    void *g3map;
    struct Categories cats;
    struct History hist;
    int head_ok;
    int cats_ok;
    int hist_ok;
    char *G_program_name();
    struct Option *opt1;
    struct Flag *rflag;
    struct GModule *module;
    double dmin, dmax;

    G_gisinit(argv[0]);
   
    module = G_define_module();
    module->description =
     _("Outputs basic information about a user-specified 3D raster map layer.");

    opt1 = G_define_option() ;
    opt1->key        = "map" ;
    opt1->type       = TYPE_STRING ;
    opt1->required   = YES ;
    opt1->gisprompt  = "old,grid3,3d raster" ;
    opt1->description= _("Name of existing 3dcell map") ;

    rflag = G_define_flag();
    rflag->key            = 'r';
    rflag->description    = _("Print range only");

    if (G_parser(argc, argv))
        exit(1);
    
    name = G_store(opt1->answer);

    if ((mapset = G_find_grid3 (name, "")) == NULL)
        G_fatal_error ( _("Cannot find %s"), name);	

    head_ok = G3d_readRegionMap (name, mapset, &cellhd) >= 0;
    hist_ok = 0;
    cats_ok = G3d_readCats (name, mapset, &cats) >= 0;

    out = stdout;

  if (!rflag->answer)
  {
    divider ('+');

    sprintf (line, "Layer:    %-29.29s  Date: %s", name, hist_ok ? hist.mapid : "??");
    printline (line);

    sprintf (line, "Mapset:   %-29.29s  Login of Creator: %s", mapset, hist_ok ? hist.creator : "??");
    printline (line);

    sprintf (line, "Location: %s", G_location());
    printline (line);

    sprintf (line, "DataBase: %s", G_gisdbase());
    printline (line);

    sprintf (line, "Title:    %s ( %s )", cats_ok ? cats.title : "??", hist_ok ? hist.title : "??");
    printline (line);

    divider ('|');
    printline ("");

    sprintf (line, "  Type of Map:  %-20.20s", "3d cell");
    strcat (line, "Number of Categories: ");
    if (cats_ok)
    {
        char temp[20];
        sprintf (temp, "%-9ld", (long)cats.num);
        strcat (line, temp);
    }
    else
        strcat (line, "??");
    printline (line);

    if (head_ok)
    {
        sprintf (line, "  Rows:         %d", cellhd.rows);
	printline (line);

        sprintf (line, "  Columns:      %d", cellhd.cols);
	printline (line);

        sprintf (line, "  Depths:       %d", cellhd.depths);
	printline (line);

        sprintf (line, "  Total Cells:  %ld", 
		 (long)cellhd.rows * cellhd.cols * cellhd.depths);
	printline (line);

	sprintf (line, "       Projection: %s (zone %d)",
	    G_database_projection_name(), G_zone());
        printline (line);

	G_format_northing (cellhd.north, tmp1, cellhd.proj);
	G_format_northing (cellhd.south, tmp2, cellhd.proj);
	G_format_resolution (cellhd.ns_res, tmp3, cellhd.proj);
        sprintf (line, "           N: %10s    S: %10s   Res: %5s",
	    tmp1, tmp2, tmp3);
        printline (line);

	G_format_easting (cellhd.east, tmp1, cellhd.proj);
	G_format_easting (cellhd.west, tmp2, cellhd.proj);
	G_format_resolution (cellhd.ew_res, tmp3, cellhd.proj);
        sprintf (line, "           E: %10s    W: %10s   Res: %5s",
	    tmp1, tmp2, tmp3);
        printline (line);

    	format_double (cellhd.top, tmp1);
	format_double (cellhd.bottom, tmp2);
	format_double (cellhd.tb_res, tmp3);
        sprintf (line, "           T: %10s    B: %10s   Res: %5s",
	    tmp1, tmp2, tmp3);
        printline (line);
    }

    printline ("");

    if (hist_ok)
    {
        printline ("  Data Source:");
        sprintf (line, "   %s", hist.datsrc_1);
	printline(line);
        sprintf (line, "   %s", hist.datsrc_2);
	printline(line);
	printline("");

        printline ("  Data Description:");
        sprintf (line, "   %s", hist.keywrd);
	printline(line);
	printline("");
        if(hist.edlinecnt)
	{
	    printline ("  Comments:  ");

	    for (i = 0; i < hist.edlinecnt; i++)
	    /**************************************/
	    {
		sprintf (line, "   %s", hist.edhist[i]);
		printline(line);
	    }
        }

        printline ("");
    }

    divider ('+');

    fprintf(out,"\n");
   }
   else /* rflag */
   {
     if (rflag->answer){

        g3map = G3d_openCellOld (name, mapset, G3D_DEFAULT_WINDOW,
                                 G3D_TILE_SAME_AS_FILE, G3D_USE_CACHE_DEFAULT);

        if(NULL == g3map)
           G_fatal_error( _("Error opening grid3 file [%s]"), name);
        if(0 == G3d_range_load(g3map))
           G_fatal_error( _("Error reading range for [%s]"), name);

        G3d_range_min_max (g3map, &dmin, &dmax);
        fprintf(out, "min=%f\n", dmin);
        fprintf(out, "max=%f\n", dmax);
     }
   }

   return 0;
}

int
format_double (double value, char *buf)
{
    sprintf (buf, "%.8lf", value);
    G_trim_decimal (buf);
    return 0;
}
