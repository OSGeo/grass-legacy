#include "math.h"
#include "gis.h"
#include "Vect.h"
#include "dig_atts.h"
#include "optri.h"

/*--------------------------------------------------------------------------*/

static void
read_sites (name, g, nofDeci)

     char *name;
     void **g;

     int nofDeci;
{
  char *mapset;
  FILE *fd;
  double east, north;
  char *desc;
  double z;
  int count, errors;
  void *s;

  mapset = G_find_sites (name,"");
  if (mapset == NULL) {
    fprintf (stderr, "%s: %s - sites map not found\n", G_program_name (), name);
    exit (1);
  }

  fd = G_fopen_sites_old (name,mapset);
  if (fd == NULL) {
    fprintf (stderr, "%s: %s - can't open sites map\n", G_program_name (), 
	     name);
    exit (1);
  }
    
  fprintf (stderr, "Reading sites map (%s) ...\n", name);
  
  count = 0;
  while (G_get_site (fd, &east, &north, &desc) > 0) 
    count++;
  *g = grNew (count, nofDeci);
  s = grSI (*g);

  rewind (fd);

  count = errors = 0;
  while (G_get_site (fd, &east, &north, &desc) > 0) {
    if (sscanf (desc, "%lf", &z) == 1 || sscanf (desc, "#%lf", &z) == 1) {
      count++;
      siSITEload (s, east, north, z);
    } else {
      count++;
      siSITEload (s, east, north, (double) 0);
      errors++;
    }
  }

  fclose (fd);

  fprintf (stderr, "\n");
    
  if (errors) {
    fprintf (stderr, 
	     "Warning: %s - %sdid not contain %svalid elevation values\n",
	     name, count?"some sites":"", count?"":"any");
  }
}

/*--------------------------------------------------------------------------*/

static double
MAXABS (a, b)

     double a, b;

{
  return (fabs (a) > fabs (b) ? fabs (a) : fabs (b));
}

/*--------------------------------------------------------------------------*/

static void

write_sites (out_name, s, nofDeci)
     char *out_name;
     void *s;
{
  int i, nofReduction;
  FILE *out_fd;
  double Mfac=1.0;
  double maxmax;

  for (i=1;i<=nofDeci;i++)
     Mfac=Mfac * 10.;

  Mfac=Mfac * 10.;
  nofReduction = -1;
  do {
    Mfac=Mfac / 10.;
    nofReduction++;
    maxmax = MAXABS (siMaxX (s), MAXABS (siMinX (s), MAXABS (siMaxY (s),
	     MAXABS (siMinY (s), MAXABS (siMaxZ (s), siMinZ (s))))));
  } while (maxmax * Mfac> 2147483647.0);

  if (nofReduction) {
    G_warning ("coordinates exceed maxint.\n");
    fprintf (stderr, 
	     "number of digits after decimal point reduced from %d to %d\n",
	     nofDeci, nofDeci - nofReduction);
  }

  out_fd = fopen(out_name, "w");
  if(!out_fd) G_fatal_error("Can't open output file!");

  for (i = 0; i < siNS (s); i++) {
    fprintf(out_fd, "site %d %d %d\n",  (int) (Mfac * siSITEX (s, i)), 
                                        (int) (Mfac * siSITEY (s, i)),
                                        (int) (Mfac * siSITEZ (s, i)));
  }
}

/*--------------------------------------------------------------------------*/

#undef MAXABS

/*--------------------------------------------------------------------------*/

main (argc, argv)

     int argc;
     char *argv[];

{
  void * graph;
  int nofDeci;
  struct {
    struct Option *input, *output, *precision;
  } parm;
  
  parm.input = G_define_option () ;
  parm.input->key        = "input" ;
  parm.input->type       = TYPE_STRING ;
  parm.input->required   = YES ;
  parm.input->description= "Name of input sites map" ;
  parm.input->gisprompt  = "old,site_lists,sites" ;
  
  parm.output = G_define_option () ;
  parm.output->key        = "output" ;
  parm.output->type       = TYPE_STRING ;
  parm.output->required   = YES;
  parm.output->description= "Name of output file in MinMaxer format";

  parm.precision = G_define_option () ;
  parm.precision->key        = "precision" ;
  parm.precision->type       = TYPE_INTEGER ;
  parm.precision->required   = NO;
  parm.precision->description= "Number of digits after decimal point" ;
  parm.precision->answer    = "0";

  G_gisinit (argv[0]);
  
  if (G_parser (argc, argv))
    exit (1);
  
  if (G_legal_filename (parm.input->answer) < 0) {
    fprintf (stderr, "%s=%s - illegal name\n", parm.input->key, 
	     parm.input->answer);
    exit (1);
  }

  if ((sscanf (parm.precision->answer, "%d", &nofDeci) != 1) || 
      (nofDeci < 0) || (nofDeci > 15)) {
    fprintf (stderr, "Invalid number of decimal digits: %s\n",
	     parm.precision->answer);
    exit (1);
  }

  read_sites (parm.input->answer, &graph, nofDeci);
  write_sites (parm.output->answer, grSI (graph), nofDeci);
  grDispose (graph);
}
  
