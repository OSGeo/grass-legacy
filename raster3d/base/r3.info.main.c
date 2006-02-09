/***************************************************************************
*
* MODULE:       r3.info
*
* AUTHOR(S):    Roman Waupotitsch, Michael Shapiro, Helena Mitasova, Bill Brown,
*               Lubos Mitas, Jaro Hofierka
*
* PURPOSE:      Outputs basic information about a user-specified 3D raster map layer.
*
* COPYRIGHT:    (C) 2005 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*               License (>=v2). Read the file COPYING that comes with GRASS
*               for details.
*
*****************************************************************************/

/* \todo
*    History support still not full implemented.
*    Only parts of the timestep functionality are implemented, the timzone is missed ;).
*/

/*local prototype*/
int format_double (double value, char *buf);

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <grass/gis.h>
#include <grass/G3d.h>
#include <grass/glocale.h>

#define printline(x) fprintf (out," | %-74.74s |\n",x)
#define divider(x) \
    fprintf (out," %c",x);\
    for (i = 0; i < 76; i++)\
        fprintf(out,"-");\
    fprintf (out,"%c\n",x)

#define TMP_LENGTH 100

static char *name;

/**************************************************************************/
int
main (int argc, char *argv[])
{
  char *mapset;
  char *line = NULL;
  char tmp1[TMP_LENGTH], tmp2[TMP_LENGTH], tmp3[TMP_LENGTH];
  int i;
  FILE *out;
  G3D_Region cellhd;
  void *g3map;
  struct Categories cats;
  struct History hist;
  struct TimeStamp ts;
  int head_ok;
  int cats_ok;
  int hist_ok;
  int time_ok = 0;
  char *G_program_name ();
  struct Option *opt1;
  struct Flag *rflag, *Rflag, *timestampflag;
  struct GModule *module;
  double dmin, dmax;

  G_gisinit (argv[0]);

  module = G_define_module ();
  module->description = _("Outputs basic information about a user-specified 3D raster map layer.");

  opt1 = G_define_option ();
  opt1->key = "map";
  opt1->type = TYPE_STRING;
  opt1->required = YES;
  opt1->gisprompt = "old,grid3,3d raster";
  opt1->description = _("Name of existing 3dcell map");

  Rflag = G_define_flag ();
  Rflag->key = 'R';
  Rflag->description = _("Print all, inclusive range");

  rflag = G_define_flag ();
  rflag->key = 'r';
  rflag->description = _("Print range only");

  timestampflag = G_define_flag ();
  timestampflag->key = 't';
  timestampflag->description = _("Print timestamp, (day.month.year hour:minute:seconds)");


  if (G_parser (argc, argv))
    exit (EXIT_FAILURE);

  name = G_store (opt1->answer);

  if ((mapset = G_find_grid3 (name, "")) == NULL)
    G_fatal_error (_("Cannot find %s"), name);

  head_ok = G3d_readRegionMap (name, mapset, &cellhd) >= 0;
  hist_ok = 0;
  cats_ok = G3d_readCats (name, mapset, &cats) >= 0;
  if(timestampflag->answer)
  time_ok = G_read_grid3_timestamp (name, mapset, &ts) >= 0;

  out = stdout;

  if (!rflag->answer)
    {
      divider ('+');

      if (G_asprintf (&line, "Layer:    %-29.29s  Date: %s", name, hist_ok ? hist.mapid : "??") > 0)
	printline (line);
      else
	G_fatal_error (_("Cannot allocate memory for string"));


      if (G_asprintf (&line, "Mapset:   %-29.29s  Login of Creator: %s", mapset, hist_ok ? hist.creator : "??") > 0)
	printline (line);
      else
	G_fatal_error (_("Cannot allocate memory for string"));


      if (G_asprintf (&line, "Location: %s", G_location ()) > 0)
	printline (line);
      else
	G_fatal_error (_("Cannot allocate memory for string"));

      if (G_asprintf (&line, "DataBase: %s", G_gisdbase ()) > 0)
	printline (line);
      else
	G_fatal_error (_("Cannot allocate memory for string"));

      if (G_asprintf (&line, "Title:    %s ( %s )", cats_ok ? cats.title : "??", hist_ok ? hist.title : "??") > 0)
	printline (line);
      else
	G_fatal_error (_("Cannot allocate memory for string"));

      /*This shows the TimeStamp*/
      if(time_ok && timestampflag->answer) {
      if (G_asprintf (&line, "Timestamp: %i.%i.%i %02i:%02i:%02g to %i.%i.%i %02i:%02i:%02g", 
          ts.dt[0].day, ts.dt[0].month,  ts.dt[0].year, ts.dt[0].hour, ts.dt[0].minute, ts.dt[0].second,
	  ts.dt[1].day, ts.dt[1].month,  ts.dt[1].year, ts.dt[1].hour, ts.dt[1].minute, ts.dt[1].second) > 0)
	printline (line);
      else
	G_fatal_error (_("Cannot allocate memory for string"));
      }
      
      divider ('|');
      printline ("");

      if (cats_ok)
	{
	  format_double ((double) cats.num, tmp1);
	}

      if (G_asprintf (&line, "  Type of Map:  %-20.20s Number of Categories: %-9s", "3d cell", cats_ok ? tmp1 : "??") >
	  0)
	printline (line);
      else
	G_fatal_error (_("Cannot allocate memory for string"));

      if (head_ok)
	{
	  if (G_asprintf (&line, "  Rows:         %d", cellhd.rows) > 0)
	    printline (line);
	  else
	    G_fatal_error (_("Cannot allocate memory for string"));

	  if (G_asprintf (&line, "  Columns:      %d", cellhd.cols) > 0)
	    printline (line);
	  else
	    G_fatal_error (_("Cannot allocate memory for string"));

	  if (G_asprintf (&line, "  Depths:       %d", cellhd.depths) > 0)
	    printline (line);
	  else
	    G_fatal_error (_("Cannot allocate memory for string"));

	  if (G_asprintf (&line, "  Total Cells:  %ld", (long) cellhd.rows * cellhd.cols * cellhd.depths) > 0)
	    printline (line);
	  else
	    G_fatal_error (_("Cannot allocate memory for string"));

	  if (G_asprintf (&line, "       Projection: %s (zone %d)", G_database_projection_name (), G_zone ()) > 0)
	    printline (line);
	  else
	    G_fatal_error (_("Cannot allocate memory for string"));

	  G_format_northing (cellhd.north, tmp1, cellhd.proj);
	  G_format_northing (cellhd.south, tmp2, cellhd.proj);
	  G_format_resolution (cellhd.ns_res, tmp3, cellhd.proj);
	  if (G_asprintf (&line, "           N: %10s    S: %10s   Res: %5s", tmp1, tmp2, tmp3) > 0)
	    printline (line);
	  else
	    G_fatal_error (_("Cannot allocate memory for string"));

	  G_format_easting (cellhd.east, tmp1, cellhd.proj);
	  G_format_easting (cellhd.west, tmp2, cellhd.proj);
	  G_format_resolution (cellhd.ew_res, tmp3, cellhd.proj);
	  if (G_asprintf (&line, "           E: %10s    W: %10s   Res: %5s", tmp1, tmp2, tmp3) > 0)
	    printline (line);
	  else
	    G_fatal_error (_("Cannot allocate memory for string"));

	  format_double (cellhd.top, tmp1);
	  format_double (cellhd.bottom, tmp2);
	  format_double (cellhd.tb_res, tmp3);
	  if (G_asprintf (&line, "           T: %10s    B: %10s   Res: %5s", tmp1, tmp2, tmp3) > 0)
	    printline (line);
	  else
	    G_fatal_error (_("Cannot allocate memory for string"));

	  /*If the range should be displayed like in r.info */
	  if (Rflag->answer)
	    {
	      /*To read the range, we need to open the map and call the range calculation */
	      g3map = G3d_openCellOld (name, mapset, G3D_DEFAULT_WINDOW, G3D_TILE_SAME_AS_FILE, G3D_USE_CACHE_DEFAULT);

	      if (NULL == g3map)
		G_fatal_error (_("Error opening grid3 file [%s]"), name);
	      if (0 == G3d_range_load (g3map))
		G_fatal_error (_("Error reading range for [%s]"), name);

	      G3d_range_min_max (g3map, &dmin, &dmax);
	      format_double (dmin, tmp1);
	      format_double (dmax, tmp2);

	      if (G_asprintf (&line, "  Range of data:   min = %10s max = %10s", tmp1, tmp2) > 0)
		printline (line);
	      else
		G_fatal_error (_("Cannot allocate memory for string"));

	    }


	}

      printline ("");

      if (hist_ok)
	{
	  printline ("  Data Source:");
	  if (G_asprintf (&line, "   %s", hist.datsrc_1) > 0)
	    printline (line);
	  else
	    G_fatal_error (_("Cannot allocate memory for string"));

	  if (G_asprintf (&line, "   %s", hist.datsrc_2) > 0)
	    printline (line);
	  else
	    G_fatal_error (_("Cannot allocate memory for string"));

	  printline ("");

	  printline ("  Data Description:");
	  if (G_asprintf (&line, "   %s", hist.keywrd) > 0)
	    printline (line);
	  else
	    G_fatal_error (_("Cannot allocate memory for string"));

	  printline ("");
	  if (hist.edlinecnt)
	    {
	      printline ("  Comments:  ");

	      for (i = 0; i < hist.edlinecnt; i++)
	    /**************************************/
		{
		  if (G_asprintf (&line, "   %s", hist.edhist[i]) > 0)
		    printline (line);
		  else
		    G_fatal_error (_("Cannot allocate memory for string"));

		}
	    }

	  printline ("");
	}

      divider ('+');

      fprintf (out, "\n");
    }
  else				/* rflag */
    {
      if (rflag->answer)
	{

	  g3map = G3d_openCellOld (name, mapset, G3D_DEFAULT_WINDOW, G3D_TILE_SAME_AS_FILE, G3D_USE_CACHE_DEFAULT);

	  if (NULL == g3map)
	    G_fatal_error (_("Error opening grid3 file [%s]"), name);
	  if (0 == G3d_range_load (g3map))
	    G_fatal_error (_("Error reading range for [%s]"), name);

	  G3d_range_min_max (g3map, &dmin, &dmax);
	  fprintf (out, "min=%f\n", dmin);
	  fprintf (out, "max=%f\n", dmax);
	}
    }

  return 0;
}

/**************************************************************************/
int format_double (double value, char *buf)
{

  sprintf (buf, "%.8lf", value);
  G_trim_decimal (buf);
  return 0;
}
