#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "gis.h"
#include "glocale.h"
#define GLOBAL
#include "kappa.h"

int
main (argc, argv)
 int argc;
 char *argv[];
{
 int i;
 struct GModule *module;
 struct { 
   struct Option *map, *ref, *output, *titles;
 } parms;

 struct { 
   struct Flag *n, *w, *q, *h;
 } flags;

 G_gisinit(argv[0]);

 module = G_define_module();
 module->description =
  _("Calculate error matrix and kappa "
  "parameter for accuracy assessment of classification "
  "result.");

 parms.map = G_define_option();
 parms.map->key		="classification";
 parms.map->type	=TYPE_STRING;
 parms.map->required	=YES;
 parms.map->description	=_("File name of classification result");

 parms.ref = G_define_option();
 parms.ref->key		="reference";
 parms.ref->type	=TYPE_STRING;
 parms.ref->required	=YES;
 parms.ref->description	=_("File name of reference classes");

 parms.output = G_define_option();
 parms.output->key		="output";
 parms.output->type		=TYPE_STRING;
 parms.output->required		=NO;
 parms.output->description 	=_("Name of an output file containing of error matrix and kappa");

 parms.titles = G_define_option();
 parms.titles->key		="title";
 parms.titles->type		=TYPE_STRING;
 parms.titles->required		=NO;
 parms.titles->description	=_("Title for error matrix and kappa");
 parms.titles->answer		="ACCURACY ASSESSMENT";

 flags.w = G_define_flag();
 flags.w->key = 'w';
 flags.w->description = _("wide report, 132 columns (default: 80)");

 flags.q = G_define_flag();
 flags.q->key	= 'q';
 flags.q->description = _("quiet");

 flags.h = G_define_flag();
 flags.h->key = 'h';
 flags.h->description = _("no header in the report");

 if (G_parser(argc, argv))
   exit (-1);

 G_get_window (&window);

 maps[0] = parms.ref->answer;
 maps[1] = parms.map->answer;
 for (i=0; i<2; i++)
   layer (maps[i]);
 
 if (parms.output->answer) {
   output=parms.output->answer;
   if (G_legal_filename (output) < 0) {
     fprintf (stderr, "\nERROR: <%s> - illegal output file name\n", 
	parms.output->answer);
     G_usage();
     exit(1);
   }
 }
 else
  output = NULL;

 title = parms.titles->answer;

 verbose = !flags.q->answer;

/* run r.stats to obtain statistics of map layers */
 stats();

/* print header of the output */
 if (!flags.h->answer)
   prn_header();

/* prepare the data for calculation */
 prn_error_mat(flags.w->answer?132:80, flags.h->answer);

/* generate the error matrix, kappa and variance */
 calc_kappa();
 
 return 0;
}

layer (s)
char *s;
{
  char msg[100], name[200], *mapset;
  int n;

  strcpy (name, s);
  if ((mapset = G_find_cell2 (name, "")) == NULL) {
    sprintf (msg, "%s: <%s> raster file not found\n", G_program_name(), s);
    G_fatal_error (msg);
  }
  n = nlayers++;
  layers = (LAYER *) G_realloc(layers, 2*sizeof(LAYER));
  layers[n].name = G_store (name);
  layers[n].mapset = mapset;
  G_read_cats (name, mapset, &layers[n].labels);
} 
