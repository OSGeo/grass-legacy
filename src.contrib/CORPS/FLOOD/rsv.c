/*******************************************************************************                         r.surf.voronoi
               Mother Earth Systems, Boulder, Colorado


This software was been developed for the U.S. Army Corps of Engineers,
Omaha District under contract #DACW45-92-P-1301.

This code is in the public domain.  Permission to use, copy, modify, and
distribute this software and its documentation for any purpose and without
fee is granted.

*******************************************************************************/
#include <stdio.h>
#include <string.h>
#include <math.h>
#include "voronoi.h"
#include "gis.h"
#include "Vect.h"
#include "rsv.h"

/*---------*/
/* Globals */
/*---------*/
char input_rname[FNAMELEN];
char output_rname[FNAMELEN];

int input_rid;
int output_rid;

struct Cell_head wind;
int    *elev;
float **vsite;
double *area;

/*============================================================================*/
main (argc, argv)
   char **argv;
   int argc;
{
   int nsites;

   /*-----------------*/
   /* Initializations */
   /*-----------------*/
   G_gisinit( argv[0] );
   init_graphics( &wind );

   /*----------------------*/
   /* Command line parsing */
   /*----------------------*/
   command_line( argc, argv );

   /*------------------------*/
   /* get & open input files */
   /*------------------------*/
   get_input();

   /*-------------------------------------------*/
   /* load voronoi sites from cross section map */
   /*-------------------------------------------*/
   nsites = readsites( input_rid );
   load_vsites( nsites, vsite, (float)wind.west, (float)wind.south, 
                               (float)wind.east, (float)wind.north );

   /*-------------------------------------*/
   /* calculate areas for voronoi regions */
   /*-------------------------------------*/
   calc_areas( nsites );

   /*-----------------------------------*/
   /* calculate interpolated raster map */
   /*-----------------------------------*/
   interpolate( nsites );

   /*----------*/
   /* clean up */
   /*----------*/
   G_close_cell( input_rid );
   G_close_cell( output_rid );
}

/*============================================================================*/
command_line( argc, argv )
   int argc;
   char **argv;
{
   struct
   {
     struct Flag *all, *points, *delaunay, *label, *q;
   } flag;
 
   struct
   {
     struct Option *input, *output, *thresh;
   } parm;

   parm.input = G_define_option ();
   parm.input->key = "input";
   parm.input->type = TYPE_STRING;
   parm.input->required = YES;
   parm.input->description = "name of a raster file to be interpolated";
   parm.input->gisprompt = "old,cell,raster,input";

   parm.output = G_define_option ();
   parm.output->key = "output";
   parm.output->type = TYPE_STRING;
   parm.output->required = YES;
   parm.output->description = "name of a raster file to be output";
   parm.output->gisprompt = "new,cell,raster,output";
 
   if (G_parser (argc, argv))
      exit (1);

   strcpy( input_rname, parm.input->answer );
   strcpy( output_rname, parm.output->answer );
}

/*============================================================================*/
get_input()
{
   char errmsg[BUFFLEN];
   char *mapset;

   /*------------------------*/
   /* open output raster map */
   /*------------------------*/
   output_rid = G_open_cell_new( output_rname ); 
   if ( output_rid < 0 ) 
   {
      sprintf (errmsg, "Not able to open raster file <%s>\n",
               output_rname);
      G_fatal_error (errmsg);
   }

   /*------------------------------------------*/
   /* open input raster map to be interpolated */
   /*------------------------------------------*/
   mapset = G_find_file ("cell", input_rname, "");
   if (mapset == NULL)
   {
      sprintf (errmsg, "raster file [%s] not found", input_rname);
      G_fatal_error (errmsg);
   }

   input_rid = G_open_cell_old( input_rname, mapset );
   if ( input_rid < 0 )
   {
      sprintf (errmsg, "can't open raster file [%s]", input_rname);
      G_fatal_error (errmsg);
   }
}

/*============================================================================*/
