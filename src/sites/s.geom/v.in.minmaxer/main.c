#ifndef linux
#include <search.h>
#endif
#include "gis.h"
#include "Vect.h"
#include "dig_atts.h"
#include "optri.h"

/*--------------------------------------------------------------------------*/

static struct Map_info OutMap;
static FILE *OutAtt, *In;
static struct line_pnts *Points;
static double div_f; /* a factor by which to divide all coordinates*/
                     /* if ran s.out.minmaxer with precision=n, then div_f=10^n"

/*--------------------------------------------------------------------------*/

static void
initGlobal (name, out_name)

     char *name, *out_name;

{
  char *mapset;
  int level;

  Points = Vect_new_line_struct ();

  In = fopen(name, "r");
  if(!In)
    G_fatal_error ("Can't open input file");
    
  if (0 > Vect_open_new (&OutMap, out_name)) {
    fprintf (stderr, "Not able to open vector file <%s>\n", out_name) ;
    exit (1);
  }

  if ((OutAtt = G_fopen_new("dig_att", out_name)) == NULL) {
    fprintf (stderr, "Could not write to new attribute file\n");
    exit (1);
  }
}

/*--------------------------------------------------------------------------*/

static void
readMinmaxer (g, fname)

     void **g;
     char *fname;
{
  int count;
  void *s, *q;

  fprintf (stderr, "Reading minmaxer input ...\n");
  
  *g = grMake ();

  grReadGraph (fname, *g);
  fprintf(stderr, "fname: %s\n", fname);
  s = grSI (*g);
  q = grQE (*g);
  fprintf(stderr, "Done Reading minmaxer input\n");
}

/*--------------------------------------------------------------------------*/

static void 
writeLineTriang (q, s, edge)

     void *q, *s;
     indexType edge;


{
  double x[2], y[2];
  indexType org, dst, dstnxt, dstprv, qe;

  qe = qeMAKEQE (edge);

  org = qeORG (q, qe);
  dst = qeDST (q, qe);
  dstnxt = qeDST (q, qeONEXT (q, qe));
  dstprv = qeDST (q, qeOPREV (q, qe));
  
  x[0] = siSITEX (s, org)/div_f; y[0] = siSITEY (s, org)/div_f;
  x[1] = siSITEX (s, dst)/div_f; y[1] = siSITEY (s, dst)/div_f;
  Vect_copy_xy_to_pnts (Points, x, y, 2);
  Vect_write_line (&OutMap, AREA, Points);
}

/*--------------------------------------------------------------------------*/

static void
writeTriangulation (q, s)

     void *q, *s;

{
  indexType edge, site;

  for (edge = 0; edge < qeNE (q); edge++)
    writeLineTriang (q, s, edge);
}

/*--------------------------------------------------------------------------*/

static void
copyHeader (out_name, s)

     char *out_name;
     void *s;

{
  struct dig_head header;
  char date[40], mon[4];
  int day, yr;

  sprintf(date,"%s",G_date());
  sscanf(date,"%*s%s%d%*s%d",mon,&day,&yr);
  if (yr < 2000) yr = yr - 1900;
  else yr = yr - 2000;
  sprintf(date,"%s %d %d",mon,day,yr);

  strcpy(header.date,date);
  strcpy(header.your_name, out_name);
  header.W = siMinX(s)/div_f;
  header.E = siMaxX(s)/div_f;
  header.S = siMinY(s)/div_f;
  header.N = siMaxY(s)/div_f;

  Vect_copy_head_data (&header, &(OutMap.head));
}

/*--------------------------------------------------------------------------*/

static void
closeGlobal ()

{
  fclose(In);
  fclose (OutAtt);
  Vect_close (&OutMap);
  Vect_destroy_line_struct (Points);
}

/*--------------------------------------------------------------------------*/

static void
write_vect (out_name, q, s)

     char * out_name;
     void *q, *s;

{
  fprintf (stderr, "Writing vector map ...\n");

  copyHeader (out_name, s);

  writeTriangulation (q, s);

  fprintf (stderr, "\nStatistics: sites %d, edges %d\n",
	   siNS (s), qeNE (q));

}

/*--------------------------------------------------------------------------*/

main (argc, argv)

     int argc;
     char *argv[];

{
  void * graph;
  struct {
    struct Option *input, *output, *div_factor;
  } parm;
  
  int i;
  
  parm.input = G_define_option () ;
  parm.input->key        = "input" ;
  parm.input->type       = TYPE_STRING ;
  parm.input->required   = YES ;
  parm.input->description= "Name of input Minmaxer file" ;
  
  parm.output = G_define_option () ;
  parm.output->key        = "output" ;
  parm.output->type       = TYPE_STRING ;
  parm.output->required   = YES;
  parm.output->description= "Name of output vector map";
  parm.output->gisprompt  = "any,dig,vector";

  parm.div_factor = G_define_option () ;
  parm.div_factor->key        = "divisor" ;
  parm.div_factor->type       = TYPE_DOUBLE ;
  parm.div_factor->required   = NO;
  parm.div_factor->description= "a number by which to divide coordinates" ;
  parm.div_factor->answer    = "1.0";

  G_gisinit (argv[0]);
  
  if (G_parser (argc, argv))
    exit (1);
  
  if (G_legal_filename (parm.output->answer) < 0) {
    fprintf (stderr, "%s=%s - illegal name\n", parm.output->key, 
	     parm.output->answer);
    exit (1);
  }

  if ((sscanf (parm.div_factor->answer, "%lf", &div_f) != 1) || 
      (div_f <= 0.0)) {
    fprintf (stderr, "Invalid division factor: %s\n",
	     parm.div_factor->answer);
    exit (1);
  }

  initGlobal (parm.input->answer, parm.output->answer);

  readMinmaxer (&graph, parm.input->answer);

  write_vect (parm.output->answer, grQE (graph), grSI (graph), 1);

  closeGlobal ();
  grDispose (graph);
}
  
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
