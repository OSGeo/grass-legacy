/*
* $Id$
*
****************************************************************************
*
* MODULE:       r.support (GRASS core)
* AUTHOR(S):    Original Michael  Shapiro - CERL
*               Preliminary parser support by Markus Neteler
* PURPOSE:      Build support file for raster map
*               - Edit the header
*               - Update the stats (histogram,range)
*               - Edit the category fil
*               - Create/Update the color table
*               - Edit the history file
*               - create/reset null file
*               - delete null file
* COPYRIGHT:    (C) 2000 by the GRASS Development Team
*
*               This program is free software under the GNU General Public 
*               License (>=v2). Read the file COPYING that comes with GRASS
*               for details.
*
*****************************************************************************/

#include <unistd.h>
#include <stdlib.h>
#include "gis.h"
#include "local_proto.h"

int main (int argc, char *argv[])
{
    char *name;
    char rname[256], rmapset[256];
    struct Cell_head cellhd;
    int row, col, null_fd;
    char element[300], path[400];
    unsigned char *null_bits;
    char buf[1024];
    int cellhd_ok;
    int is_reclass;
    int error();
    int calc_range;
    int commandmode;
    struct GModule *module;
    struct Option *map;
    struct Flag *flag1;          

    G_gisinit (argv[0]);

    module = G_define_module();
    module->description =
		"Allows the user to create and/or modify raster map layer "
		"support files. Note: Interactive mode offers more functionality than "
		"command line mode.";

    map = G_define_option();            
    map->key = "map";
    map->required = YES;
    map->type = TYPE_STRING;
    map->gisprompt = "old,cell,raster";
    map->description = "raster map name";

    /* test if running in command line mode */
    if (argc >2 ) /* -r is specified */
    {
      flag1 = G_define_flag() ;
      flag1->key         = 'r';
      flag1->description = "Calculate range";
      commandmode=1; /* Calculate range wanted == run in command mode */
    }
    else
      commandmode=0; /* Calculate range not wanted */
    

    if (G_parser(argc,argv))
            exit(1);

    name = map->answer;
    if (commandmode)
        calc_range= flag1->answer;

/* cell header */
    cellhd_ok = G_get_cellhd (name, G_mapset(), &cellhd) >= 0 ;
    is_reclass = (G_is_reclass (name, G_mapset(), rname, rmapset) > 0);

  if (! commandmode)
  {
    sprintf (buf, "Edit the header for [%s]? ", name);
    if (is_reclass)
    {
	fprintf (stdout,"\nNote: [%s] is a reclass of [%s in %s]\n\n",
		name, rname, rmapset);
    }
    else if (G_yes (buf, cellhd_ok?0:1))
    {
	G_clear_screen();
	sprintf (buf, "%s/etc/modhead - '%s'", G_gisbase(), name);
	system (buf);
	if(cellhd_ok = G_get_cellhd (name, G_mapset(), &cellhd) >= 0)
	{
	    hitreturn();
	    G_clear_screen();
	}
    }
    if (!cellhd_ok)
    {
	fprintf (stdout,"Can't continue\n");
	sleep(3);
	exit(1);
    }
   }
   
/* check the histogram and range */
  check_stats (name, commandmode);

/* category file */
  if (! commandmode)
  {
    sprintf (buf, "Edit the category file for [%s]? ", name);
    if (G_yes (buf, 0))
    {
	G_clear_screen();
	sprintf (buf, "%s/etc/modcats '%s'", G_gisbase(), name);
	system (buf);
	hitreturn();
	G_clear_screen();
    }
   }
   
/* color table */
  if (! commandmode)
  {
    sprintf (buf, "Create/Update the color table for [%s]? ", name);
    if (G_yes (buf, 0))
    {
	G_clear_screen();
	sprintf (buf, "%s/etc/modcolr '%s'", G_gisbase(), G_fully_qualified_name(name, G_mapset()));
	system (buf);
	hitreturn();
	G_clear_screen();
    }
  }
  
/* history file */
  if (! commandmode)
  {
    sprintf (buf, "Edit the history file for [%s]? ", name);
    if (G_yes (buf, 0))
    {
	G_clear_screen();
	sprintf (buf, "%s/etc/modhist '%s'", G_gisbase(), name);
	system (buf);
	hitreturn();
	G_clear_screen();
    }
  }
  
/* null file */
  if (! commandmode)
  {
    fprintf(stdout, "\nThe null file for [%s] might indicate that some cells contain\n", name);
    fprintf(stdout, "no data. If null file for [%s] doesn't exist all zero cells in [%s]\n", name, name);
    fprintf(stdout, "are treated by GRASS application programs as no data.\n");
    sprintf (buf, "\nDo you want to create/reset null file for [%s] so that\nall cell values are considered valid data? ", name);
    if (G_yes (buf, 0))
    {
	G_clear_screen();
	/* write a file of no-nulls */
	null_bits = (unsigned char *) G__allocate_null_bits (cellhd.cols);
	/* init all cells to 0's */
	for (col = 0; col < G__null_bitstream_size(cellhd.cols); col++)
	     null_bits[col] = 0;

        if (is_reclass)
	{
	   sprintf(buf, "%s is a reclass of another map. Exiting.", name);
	   G_fatal_error(buf);
        }

	sprintf(element,"cell_misc/%s",name);
	null_fd = G_open_new (element, "null");
 
	fprintf(stdout, "Writing new null file for [%s]... ", name);
	for(row=0; row < cellhd.rows; row++)
	{
	   G_percent (row, cellhd.rows, 1);
	   if( G__write_null_bits(null_fd, null_bits, row, cellhd.cols, 0) < 0)
	   {
	       sprintf(buf, "Error writing null row %d", row); 
	       G_fatal_error(buf);
           }
	}
	G_percent (row, cellhd.rows, 1);
	close(null_fd);

	hitreturn();
	G_clear_screen();
    }
    sprintf (buf, "\nDo you want to delete null file for [%s] \n(all zero cells will then be considered no data)? ", name);
    if (G_yes (buf, 0))
    {
        if (is_reclass)
	{
	   sprintf(buf, "%s is a reclass of another map. Exiting.", name);
	   G_fatal_error(buf);
        }

	G_clear_screen();
	/* write a file of no-nulls */
	fprintf(stdout, "Removing null file for [%s]... ", name);
	sprintf(element,"cell_misc/%s",name);
	null_fd = G_open_new (element, "null");
        G__file_name(path, element, "null", G_mapset());
	unlink (path);
	fprintf(stdout, "Done.\n");

    }
   }
   
   exit(0);
}

int G_clear_screen (void){return 0;}
