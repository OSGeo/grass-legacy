#ifndef linux
#include <search.h>
#endif
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "gis.h"
#include "Vect.h"
#include "dig_atts.h"
#include "optri.h"

/*--------------------------------------------------------------------------*/

static struct Map_info InMap, OutMap;
static FILE *OutAtt;
static struct line_pnts *Points;

/*--------------------------------------------------------------------------*/

static void 
initGlobal (char *name, char *out_name)

{
  char *mapset;
  int level;

  Points = Vect_new_line_struct ();

  mapset = G_find_vector2 (name, "");
  if (mapset == NULL) {
    fprintf (stderr, "warning: %s - vector file not found\n", name);
    exit (1);
  }

  level = Vect_open_old (&InMap, name, mapset);
  if (level < 0)
    G_fatal_error ("Can't open vector file");
  if (level < 2)
    G_fatal_error ("You must first run v.support on vector file");
    
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

static void *T;
static indexType findVertexName;
static indexType * Index;
static int nofSites;

static int 
sitesGT (void *i1, void *i2)

{
  indexType v1, v2;
  double dummy;

  v1 = *((indexType *) i1);
  v2 = *((indexType *) i2);

/*fprintf (stdout,"comparing %d %d %lf %lf\n", v1, v2, siSITEX (T, v1), siSITEY (T, v2));
fprintf (stdout,"out\n %d", sosrLexoCompare (T, v1, v2, &dummy));*/
  return sosrLexoCompare (T, v1, v2, &dummy);
}

/*--------------------------------------------------------------------------*/

static void 
initFindSites (void *s)

{
  indexType point;

  Index = (indexType *) G_malloc (sizeof (indexType) * (siNS (s) + 10));
  for (point = 0; point < siNS (s); point++)
    Index[point] = point;
  T = s;
  qsort (Index, siNS (s), sizeof (indexType), sitesGT);
  nofSites = siNS (s);
  findVertexName = siSITEload (s, (coordType) 0, (coordType) 0, 
			       (coordType) 0);
  Index[nofSites] = findVertexName;
}

/*--------------------------------------------------------------------------*/

static int 
findSite (void *s, double x, double y)

{
  void *index;
  extern void * bsearch ();


  siSITEreload (s, findVertexName, x, y, (coordType) 0);

  index = bsearch (&(Index[nofSites]), Index, nofSites, sizeof (indexType), 
		   sitesGT);

  if (index == NULL) {
    fprintf (stdout,"something went wrong\n");
    exit (1);
  }

  return *((indexType *) index);
}

/*--------------------------------------------------------------------------*/

static int loopEdgesCount;

static void 
loadEdge (void *q, void *s, double xFrom, double yFrom, double xTo, double yTo, int cat)

{
  indexType qe, edge, pFrom, pTo;

  pFrom = findSite (s, xFrom, yFrom);
  pTo = findSite (s, xTo, yTo);

  if (pFrom == pTo) {
    loopEdgesCount++;
    return;
  }
  
  qe = qeAddSiteSite (q, pFrom, pTo);
  qeSETCONSTRedge (q, qeQETOE (qe));
  
  if (cat > 0)
    write_att (OutAtt, FILE_LINE,
	       (siSITEX (s, pFrom) + siSITEX (s, pTo)) / 2.0,
	       (siSITEY (s, pFrom) + siSITEY (s, pTo)) / 2.0,
	       cat);
}

/*--------------------------------------------------------------------------*/

static void 
readInMapLine (int line)

{
  int type;

  if (0 > (type = V2_read_line (&InMap, Points, line))) 
    if (type == -1) {
      fprintf (stderr, "Out of memory on line %d\n", line);
      exit (1);
    } 
}
/*--------------------------------------------------------------------------*/

static void 
readVector (graphType **g, int nofDeci, int convexHullOnly)
{
  int count;
  void *s, *q;
  int line, node, point, lineCat;

  fprintf (stderr, "Reading vector map ...\n");
  
  count = InMap.n_nodes;
  for (line = 1; line <= InMap.n_lines; line++) {
    readInMapLine (line);
    count += Points->n_points - 1;
  }

  *g = grNew (count + 10, nofDeci);
  s = grSI (*g);
  q = grQE (*g);

  for (node = 1; node <= InMap.n_nodes; node++) 
    siSITEload (s, InMap.Node[node].x, InMap.Node[node].y, (coordType) 0);
  for (line = 1; line <= InMap.n_lines; line++) {
    readInMapLine (line);
    for (point = 1; point < Points->n_points - 1; point++) 
      siSITEload (s, Points->x[point], Points->y[point], (coordType) 0);
  }

  fprintf (stderr, "\nRemoving duplicate vertices ...\n");
  siRemoveDuplicateSites (s);

  if (! convexHullOnly) {
    initFindSites (s);
    
    fprintf (stderr, "Reading edges ...\n");
    loopEdgesCount = 0;
    for (line = 1; line <= InMap.n_lines; line++) {
      readInMapLine (line);
      lineCat = InMap.Att[InMap.Line[line].att].cat;
      for (point = 1; point < Points->n_points; point++) 
	loadEdge (q, s, Points->x[point], Points->y[point],
		  Points->x[point - 1], Points->y[point - 1], 
		  lineCat);
    }
    
    if (loopEdgesCount)
      G_warning 
	("several loops (edges with Origin == Destination) removed.\n");
    
    free (Index);
    siNSset (s, siNS (s) - 1);
    
    fprintf (stderr, "\nRemoving Degeneracies ...\n");
    grMakeNonDegenerate (*g);
  }

  fprintf (stderr, "\n");
}

/*--------------------------------------------------------------------------*/

static void 
writeLineTriang (q, s, edge, lineOnly, triangleCount, flatTriangleCount)

     void *q, *s;
     indexType edge;
     int lineOnly;
     int *triangleCount, *flatTriangleCount;


{
  double x[2], y[2];
  indexType org, dst, dstnxt, dstprv, qe;

  qe = qeMAKEQE (edge);

  org = qeORG (q, qe);
  dst = qeDST (q, qe);
  dstnxt = qeDST (q, qeONEXT (q, qe));
  dstprv = qeDST (q, qeOPREV (q, qe));
  
  x[0] = siSITEX (s, org); y[0] = siSITEY (s, org);
  x[1] = siSITEX (s, dst); y[1] = siSITEY (s, dst);
  Vect_copy_xy_to_pnts (Points, x, y, 2);
  Vect_write_line (&OutMap, AREA, Points);

  if (! lineOnly) {
    if ((dstnxt == qeDST (q, qeOPREV (q, qeSYM (qe)))) &&
	(org > dstnxt) && (dst > dstnxt)) 
      if (! sosrCollinear (s, org, dst, dstnxt))
	write_att (OutAtt, FILE_AREA, 
		   (siSITEX (s, org) + siSITEX (s, dst) + 
		    siSITEX (s, dstnxt)) / 3.0,
		   (siSITEY (s, org) + siSITEY (s, dst) + 
		    siSITEY (s, dstnxt)) / 3.0,
		   (*triangleCount)++);
      else {
	(*flatTriangleCount)++;
	(*triangleCount)++;
      }
    if ((dstprv == qeDST (q, qeONEXT (q, qeSYM (qe)))) &&
	((org > dstprv) && (dst > dstprv)))
      if (! sosrCollinear (s, org, dst, dstprv))
	write_att (OutAtt, FILE_AREA, 
		   (siSITEX (s, org) + siSITEX (s, dst) + 
		    siSITEX (s, dstprv)) / 3.0,
		   (siSITEY (s, org) + siSITEY (s, dst) + 
		    siSITEY (s, dstprv)) / 3.0,
		   (*triangleCount)++);
      else {
	(*flatTriangleCount)++;
	(*triangleCount)++;
      }
  }
}

/*--------------------------------------------------------------------------*/

static void 
writeConvexHull (void *q, void *s)

{
  indexType first, edge;
  int dummy, dummy2;

  first = qeCHQE (q);
  writeLineTriang (q, s, qeQETOE (first), 1, &dummy, &dummy2);
  for (edge = qeONEXT (q, qeSYM (first)); edge != first;
       edge = qeONEXT (q, qeSYM (edge)))
    writeLineTriang (q, s, qeQETOE (edge), 1, &dummy, &dummy2);
}

/*--------------------------------------------------------------------------*/

static void 
writeTriangulation (void *q, void *s, int *triangleCount, int *flatTriangleCount)

{
  indexType edge, site;
  double x, y;

  *triangleCount = 1;
  for (edge = 0; edge < qeNE (q); edge++)
    writeLineTriang (q, s, edge, 0, triangleCount, flatTriangleCount);
  for (site = 0; site < siNS (s); site++) 
    write_att (OutAtt, FILE_DOT, siSITEX (s, site), siSITEY (s, site),
	       (int) siSITEZ (s, site));
}

/*--------------------------------------------------------------------------*/

static void 
copyHeader (char *out_name)

{
  struct dig_head header;
  char date[40], mon[4];
  int day, yr;

  Vect_copy_head_data (&(InMap.head), &header);

  sprintf(date,"%s",G_date());
  sscanf(date,"%*s%s%d%*s%d",mon,&day,&yr);
  if (yr < 2000) yr = yr - 1900;
  else yr = yr - 2000;
  sprintf(date,"%s %d %d",mon,day,yr);

  strcpy(header.date,date);
  strcpy(header.your_name, out_name);

  Vect_copy_head_data (&header, &(OutMap.head));
}

/*--------------------------------------------------------------------------*/

static void 
closeGlobal (void)

{
  fclose (OutAtt);
  Vect_close (&OutMap);
  Vect_close (&InMap);
  Vect_destroy_line_struct (Points);
}

/*--------------------------------------------------------------------------*/

static void 
write_vect (char *out_name, void *q, void *s, int convexHullOnly, int testEuler)

{
  int triangleCount, flatTriangleCount;

  fprintf (stderr, "Writing Vector Map ...\n"); fflush (stdout);

  copyHeader (out_name);

  if (convexHullOnly) {
    triangleCount = 1;
    flatTriangleCount = 0;
    writeConvexHull (q, s);
  } else 
    writeTriangulation (q, s, &triangleCount, &flatTriangleCount);

  if ((testEuler) && (siNS (s) - qeNE (q) + triangleCount != 2))
    G_warning 
	("Warning: Euler says something is wrong with this triangulation\n");

  fprintf (stderr, "\nStatistics: sites %d, edges %d, triangles %d.\n",
	   siNS (s), qeNE (q), triangleCount - 1);

  if (flatTriangleCount)
    fprintf (stderr,
	"%d flat triangles in triangulation. See man page for directions.\n",
	     flatTriangleCount);

  fprintf (stdout," finished.\n");
}

/*--------------------------------------------------------------------------*/

int 
main (int argc, char *argv[])

{
  graphType *graph;
  int nofDeci;
  struct {
    struct Option *input, *output, *precision, *operation;
  } parm;
  
#define nofOperations 7

  static char * operations [nofOperations] = {"sweep", "delaunay", 
					      "angle", "height", "slope", 
			 	              "hull", "readwrite"};

  static int needsPlanesweep [nofOperations] = {0,1,1,1,1,1,0};

  int i;
  
  parm.input = G_define_option () ;
  parm.input->key        = "input" ;
  parm.input->type       = TYPE_STRING ;
  parm.input->required   = YES ;
  parm.input->description= "Name of input vector map" ;
  parm.input->gisprompt  = "old,dig,vector" ;
  
  parm.output = G_define_option () ;
  parm.output->key        = "output" ;
  parm.output->type       = TYPE_STRING ;
  parm.output->required   = YES;
  parm.output->description= "Name of output vector map";
  parm.output->gisprompt  = "any,dig,vector";

  parm.precision = G_define_option () ;
  parm.precision->key        = "precision" ;
  parm.precision->type       = TYPE_INTEGER ;
  parm.precision->required   = NO;
  parm.precision->description= "Number of digits after decimal point" ;
  parm.precision->answer    = "0";

  parm.operation = G_define_option () ;
  parm.operation->key        = "operation" ;
  parm.operation->type       = TYPE_STRING ;
  parm.operation->required   = NO;
  parm.operation->description= "Name of operation" ;
  parm.operation->answer     = "delaunay";
  parm.operation->options    = 
                     "sweep,delaunay,angle,height,slope,hull,readwrite";

  G_gisinit (argv[0]);
  
  if (G_parser (argc, argv))
    exit (1);
  
  if (G_legal_filename (parm.input->answer) < 0) {
    fprintf (stderr, "%s=%s - illegal name\n", parm.input->key, 
	     parm.input->answer);
    exit (1);
  }
  if (G_legal_filename (parm.output->answer) < 0) {
    fprintf (stderr, "%s=%s - illegal name\n", parm.output->key, 
	     parm.output->answer);
    exit (1);
  }

  if ((sscanf (parm.precision->answer, "%d", &nofDeci) != 1) || 
      (nofDeci < 0) || (nofDeci > 15)) {
    fprintf (stderr, "Invalid number of decimal digits: %s\n",
	     parm.precision->answer);
    exit (1);
  }
  
  initGlobal (parm.input->answer, parm.output->answer);
  
  for (i = 0; i < nofOperations; i++)
    if (strcmp (parm.operation->answer, operations [i]) == 0)
      break;
  
  if (i >= nofOperations) G_fatal_error ("Operation undefined");
  
  readVector (&graph, nofDeci, (i == 5));

  if (needsPlanesweep [i]) {
    grPlanesweepTriangulation (graph);
    grDelaunayTriangulation (graph);
    grCleanConvexHull (graph); /* reduce complexity */
  }
  
  switch (i) {
  case 0: grPlanesweepTriangulation (graph); break;
  case 1: /* grDelaunayTriangulation (graph); */ break;
  case 2: grMinMaxAngleTriangulation (graph); break;
  case 3: grMaxMinHeightTriangulation (graph); break;
  case 4: grMinMaxSlopeTriangulation (graph); break;
  case 5: /* grDelaunayTriangulation (graph); */ break;
  };
  
  if ((! needsPlanesweep [i]) && (i != 6))
    grCleanConvexHull (graph);/* this won't get rid of every flat triangle
				 since the algorithm assumes some nice
				 properties of the delaunay triangulation. */

  write_vect (parm.output->answer, grQE (graph), grSI (graph), 
	      (i == 5), (i < 5));

  closeGlobal ();
  grDispose (graph);

  return 0;
}
  
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
