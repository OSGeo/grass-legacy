#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "gis.h"
#include "Vect.h"
#include "optri.h"
#include "voroSupport.h"

/*--------------------------------------------------------------------------*/

double EAST, WEST, NORTH, SOUTH;
struct Cell_head REGION;
struct line_pnts *vLEFT, *vRIGHT, *vTOP, *vBOTTOM;

static struct line_pnts *Points;
static struct Map_info OutMap;
static FILE *OutAtt;
struct dig_head header;

#define MAX(a,b) ((a) > (b) ? (a) : (b))
#define MIN(a,b) ((a) < (b) ? (a) : (b))

/*--------------------------------------------------------------------------*/

static void
my_read_sites (name, g, nofDeci, ftype, findex)

     char *name;
     void **g;
     int nofDeci, ftype, findex;
{
  char *mapset;
  FILE *fd;
  SITE_XYZ *theSite;
  int count;
  void *s;

  G_get_window (&REGION);

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
  
  if (NULL == (theSite = G_alloc_site_xyz(1)))
	  G_fatal_error("%s: Can't allocate memory!", G_program_name());
  
  fprintf (stderr, "Reading sites map (%s) ...\n", name);
  
  count = 0;
  while (G_readsites_xyz(fd, (ftype != -1) ? ftype : SITE_COL_NUL,
			  findex, 1, &REGION, theSite) == 1) 
    count++;
  fprintf(stderr, "Found %d sites ...\n", count);
  *g = grNew (count, nofDeci);
  s = grSI (*g);

  rewind (fd);

  count = 0;
  while (G_readsites_xyz(fd, (ftype != -1) ? ftype : SITE_COL_NUL,
			  findex, 1, &REGION, theSite) == 1) {
    switch(ftype) {
        case -1:
            siSITEload(s, theSite->x, theSite->y, (double) 0.0);
            break;
        case SITE_COL_NUL:
            switch(theSite->cattype) {
                case CELL_TYPE:
                    siSITEload(s, theSite->x, theSite->y,
                            (double) theSite->cat.c);
                    break;
                case FCELL_TYPE:
                    siSITEload(s, theSite->x, theSite->y,
                            (double) theSite->cat.f);
                    break;
                case DCELL_TYPE:
                    siSITEload(s, theSite->x, theSite->y, theSite->cat.d);
                    break;
                default: /* No cat */
                    G_warning("%s: Missing category value, using 0.0",
                            G_program_name());
                    siSITEload(s, theSite->x, theSite->y, (double) 0.0);
            }
            break;
        case SITE_COL_DIM:
        case SITE_COL_DBL:
        case SITE_COL_STR:
            siSITEload(s, theSite->x, theSite->y, theSite->z);
            break;
        default:
            G_fatal_error("Programmer error!");
    }
    count++;
  }

  fclose (fd);
  G_free_site_xyz(theSite);

  fprintf (stderr, "\nRemoving duplicate vertices ...\n");
  siRemoveDuplicateSites (s);

  EAST = MAX (REGION.east, siMaxX (s));
  WEST = MIN (REGION.west, siMinX (s));
  SOUTH = MIN (REGION.south, siMinY (s));
  NORTH = MAX (REGION.north, siMaxY (s));

  fprintf (stderr, "\n");

}

/*--------------------------------------------------------------------------*/

static int
dblCmp (i, j)

    double *i, *j;

{
  if (*i < *j) return  -1;
  if (*i > *j) return 1;
  return 0;
}

/*--------------------------------------------------------------------------*/

static void
writeBoundaryEdges (topXarr, nPoints, west, east, north, isEastWest)

     double * topXarr;
     int nPoints;
     double west, east, north;
     int isEastWest;

{
  double x[2], y[2];
  int i;

  qsort (topXarr, nPoints, sizeof (double), dblCmp);

  i = 0;
  while ((i < nPoints) && (topXarr[i] < west)) {
    i++;
  }

  x[1] = west;
  y[0] = y[1] = north;

  for (i; (i < nPoints) && (topXarr[i] < east); i++) 
    if (topXarr[i] != x[1]) {
      x[0] = x[1];
      x[1] = topXarr[i];
      if (isEastWest)
	Vect_copy_xy_to_pnts (Points, x, y, 2);
      else
	Vect_copy_xy_to_pnts (Points, y, x, 2);
      Vect_write_line (&OutMap, AREA, Points);
    }
  
  x[0] = east;
  if (isEastWest)
    Vect_copy_xy_to_pnts (Points, x, y, 2);
  else
    Vect_copy_xy_to_pnts (Points, y, x, 2);
  Vect_write_line (&OutMap, AREA, Points);
}

/*--------------------------------------------------------------------------*/

static void
write_vect_voronoi (out_name, q, s)

     char *out_name;
     void *q, *s;

{
  indexType edge, qe, org, dst, dstnxt, dstprv, site;
  int triangleCount, i;
  double x[2];
  double y[2];
  char date[40], mon[4];
  int day, yr;

  fprintf (stdout,"\nWriting Voronoi Diagram ..."); fflush (stdout);

  if (0 > Vect_open_new (&OutMap, out_name)) {
    fprintf (stderr, "Not able to open vector file <%s>\n", out_name) ;
    exit (1);
  }

  if ((OutAtt = G_fopen_new("dig_att", out_name)) == NULL) {
    fprintf (stderr, "Could not write to new attribute file\n");
    exit (1);
  }
  
  sprintf(date,"%s",G_date());
  sscanf(date,"%*s%s%d%*s%d",mon,&day,&yr);
  if (yr < 2000) yr = yr - 1900;
  else yr = yr - 2000;
  sprintf(date,"%s %d %d",mon,day,yr);

  strcpy(header.date,date);
  strcpy(header.your_name, out_name);
  header.plani_zone = G_zone ();

  header.N = NORTH; header.S = SOUTH;
  header.E = EAST; header.W = WEST;

  Vect_copy_head_data (&header, &(OutMap.head));
  
  Points = Vect_new_line_struct ();
  vLEFT = Vect_new_line_struct ();
  vRIGHT = Vect_new_line_struct ();
  vTOP = Vect_new_line_struct ();
  vBOTTOM = Vect_new_line_struct ();

  for (edge = 0; edge < qeNE (q); edge++) {
    if (computeVoronoiEdge (q, s, edge, &(x[0]), &(y[0]), &(x[1]), &(y[1]))) {
      Vect_copy_xy_to_pnts (Points, x, y, 2);
      Vect_write_line (&OutMap, AREA, Points);
    }
  }
  
  writeBoundaryEdges (vTOP->x, vTOP->n_points, WEST, EAST, NORTH, 1);
  writeBoundaryEdges (vBOTTOM->x, vBOTTOM->n_points, WEST, EAST, SOUTH, 1);
  writeBoundaryEdges (vLEFT->y, vLEFT->n_points, SOUTH, NORTH, WEST, 0);
  writeBoundaryEdges (vRIGHT->y, vRIGHT->n_points, SOUTH, NORTH, EAST, 0);

  for (i = 0; i < siNS (s); i++) 
    write_att (OutAtt, FILE_AREA, siSITEX (s, i), siSITEY (s, i),
	       (int) siSITEZ (s, i));

  Vect_close (&OutMap);
  fclose (OutAtt);
  Vect_destroy_line_struct (Points);
  Vect_destroy_line_struct (vTOP);
  Vect_destroy_line_struct (vBOTTOM);
  Vect_destroy_line_struct (vRIGHT);
  Vect_destroy_line_struct (vLEFT);
  fprintf (stdout," finished.\n");
}

/*--------------------------------------------------------------------------*/

static void
write_vect (out_name, q, s, doConvexHull)

     char *out_name;
     void *q, *s;
     int doConvexHull;

{
  indexType edge, qe, org, dst, dstnxt, dstprv, site, first;
  int triangleCount, flatTriangleCount, i;
  double x[2];
  double y[2];
  char date[40], mon[4], tmp_buf[100];
  int day, yr;
  struct Categories cats;

  fprintf (stdout,"\nWriting Vector Map ..."); fflush (stdout);

  if (0 > Vect_open_new (&OutMap, out_name)) {
    fprintf (stderr, "Not able to open vector file <%s>\n", out_name) ;
    exit (1);
  }

  if ((OutAtt = G_fopen_new("dig_att", out_name)) == NULL) {
    fprintf (stderr, "Could not write to new attribute file\n");
    exit (1);
  }

  sprintf(date,"%s",G_date());
  sscanf(date,"%*s%s%d%*s%d",mon,&day,&yr);
  if (yr < 2000) yr = yr - 1900;
  else yr = yr - 2000;
  sprintf(date,"%s %d %d",mon,day,yr);
  
  strcpy(header.date,date);
  strcpy(header.your_name, out_name);
  header.plani_zone = G_zone ();

  header.N = NORTH; header.S = SOUTH;
  header.E = EAST; header.W = WEST;

  Vect_copy_head_data (&header, &(OutMap.head));
  
  Points = Vect_new_line_struct ();

  if (doConvexHull) {
    first = qeCHQE (q);
    org = qeORG (q, first);
    dst = qeDST (q, first);
    x[0] = siSITEX (s, org); y[0] = siSITEY (s, org);
    x[1] = siSITEX (s, dst); y[1] = siSITEY (s, dst);
    Vect_copy_xy_to_pnts (Points, x, y, 2);
    Vect_write_line (&OutMap, AREA, Points);
    for (qe = qeONEXT (q, qeSYM (first)); qe != first;
	 qe = qeONEXT (q, qeSYM (qe))) {
      org = qeORG (q, qe);
      dst = qeDST (q, qe);
      x[0] = siSITEX (s, org); y[0] = siSITEY (s, org);
      x[1] = siSITEX (s, dst); y[1] = siSITEY (s, dst);
      Vect_copy_xy_to_pnts (Points, x, y, 2);
      Vect_write_line (&OutMap, AREA, Points);
    }
  } else {
    G_init_cats (qeNE (q) - siNS (s) + 2, out_name, &cats);
    triangleCount = 1;
    flatTriangleCount = 0;
    for (edge = 0; edge < qeNE (q); edge++) {
      qe = qeMAKEQE (edge);
      org = qeORG (q, qe);
      dst = qeDST (q, qe);
      x[0] = siSITEX (s, org); y[0] = siSITEY (s, org);
      x[1] = siSITEX (s, dst); y[1] = siSITEY (s, dst);
      Vect_copy_xy_to_pnts (Points, x, y, 2);
      Vect_write_line (&OutMap, AREA, Points);
      
      dstnxt = qeDST (q, qeONEXT (q, qe));
      dstprv = qeDST (q, qeOPREV (q, qe));
      
      if ((dstnxt == qeDST (q, qeOPREV (q, qeSYM (qe)))) &&
	  (org > dstnxt) && (dst > dstnxt)) 
	if (! sosrCollinear (s, org, dst, dstnxt))
	  write_att (OutAtt, FILE_AREA, 
		     (siSITEX (s, org) + siSITEX (s, dst) + 
		      siSITEX (s, dstnxt)) / 3.0,
		     (siSITEY (s, org) + siSITEY (s, dst) + 
		      siSITEY (s, dstnxt)) / 3.0,
		     triangleCount++);
	else {
	  flatTriangleCount++;
	  triangleCount++;
	}
      if ((dstprv == qeDST (q, qeONEXT (q, qeSYM (qe)))) &&
	  ((org > dstprv) && (dst > dstprv)))
	if (! sosrCollinear (s, org, dst, dstprv))
	  write_att (OutAtt, FILE_AREA, 
		     (siSITEX (s, org) + siSITEX (s, dst) + 
		      siSITEX (s, dstprv)) / 3.0,
		     (siSITEY (s, org) + siSITEY (s, dst) + 
		      siSITEY (s, dstprv)) / 3.0,
		     triangleCount++);
	else {
	  flatTriangleCount++;
	  triangleCount++;
	}
      sprintf(tmp_buf,"%f  %f  %f", siSITEZ (s, org), siSITEZ (s, dst), 
	      siSITEZ (s, dstprv));
      G_set_cat (triangleCount-1, tmp_buf, &cats);
    }
    fprintf (stderr, "\nTriangulation: sites %d, edges %d, triangles %d.\n",
	   siNS (s), qeNE (q), triangleCount - 1);
    if (flatTriangleCount)
      fprintf (stderr, 
          "%d flat triangles in triangulation. See man page for directions.\n",
	       flatTriangleCount);

    if (siNS (s) - qeNE (q) + triangleCount != 2) 
      G_warning
	("Warning: Euler says something is wrong with this triangulation\n");
  }

  Vect_close (&OutMap);
  fclose (OutAtt);
  if(G_write_vector_cats(out_name, &cats)<0)
       G_warning("cannot write category file\n");

  fprintf (stdout," finished.\n");
}

/*--------------------------------------------------------------------------*/

int
main (argc, argv)

     int argc;
     char *argv[];

{
  void * graph;
  int nofDeci, field_type, field_index;
  struct {
    struct Option *input, *output, *zfield, *zindex, *precision, *operation;
  } parm;
  
  extern void convexHull ();
  extern void voronoi ();

#define nofOperations 8

  static char * operations [nofOperations] = {"sweep", "delaunay", 
					      "angle", "height", "slope", 
			 	              "regular", "hull", "voronoi"};

  static int needsPlanesweep [nofOperations] = {0,1,1,1,1,0,1,1};

  int i;

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
  parm.output->description= "Name of output vector map";
  parm.output->gisprompt  = "any,dig,vector";

  parm.zfield = G_define_option();
  parm.zfield->key        = "zfield" ;
  parm.zfield->type       = TYPE_STRING;
  parm.zfield->required   = NO ;
  parm.zfield->options    = "none,dim,cat,decimal,string" ;
  parm.zfield->answer     = "none" ;
 
  parm.zindex = G_define_option();
  parm.zindex->key        = "zindex" ;
  parm.zindex->type       = TYPE_INTEGER;
  parm.zindex->required   = NO;
  parm.zindex->description = "Field index for 'zfield'";
  parm.zindex->answer     = "1";

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
  parm.operation->options    = "sweep,delaunay,angle,height,slope,regular,hull,voronoi";

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

  if ((strncmp(parm.zfield->answer, "none", 4)) == 0)
	  field_type = -1;
  else if ((strncmp(parm.zfield->answer, "dim", 3)) == 0)
	  field_type = SITE_COL_DIM;
  else if ((strncmp(parm.zfield->answer, "cat", 3)) == 0)
	  field_type = SITE_COL_NUL;
  else if ((strncmp(parm.zfield->answer, "decimal", 7)) == 0)
	  field_type = SITE_COL_DBL;
  else if ((strncmp(parm.zfield->answer, "string", 6)) == 0)
	  field_type = SITE_COL_STR;
  else
	  G_fatal_error("%s: Unknown field type %s", G_program_name(),
			  parm.zfield->answer);

  field_index = atoi(parm.zindex->answer);
  
  switch(field_type) {
	  case -1:
	  case SITE_COL_NUL:
		  field_index = -1; break;
	  case SITE_COL_DIM:
		  if (field_index < 3)
			  G_fatal_error("%s: Dimension column index must "
				"be greater than 2", G_program_name());
		  field_index -= 3; break;
          case SITE_COL_DBL:
	  case SITE_COL_STR:
		  if (field_index < 1)
			  G_fatal_error("%s: Field index must be greater "
				"than zero.", G_program_name());
		  field_index--; break;
	  default: /* Nada */
  }
  
  my_read_sites (parm.input->answer, &graph, nofDeci, 
		  field_type, field_index);

  for (i = 0; i < nofOperations; i++)
    if (strcmp (parm.operation->answer, operations [i]) == 0)
      break;
      
  if (i >= nofOperations) 
    G_fatal_error ("Operation undefined");

  if (needsPlanesweep [i]) {
    grPlanesweepTriangulation (graph);
    grDelaunayTriangulation (graph); /* need this for the next */
    grCleanConvexHull (graph); /* reduce complexity */
  }

  switch (i) {
  case 0: grPlanesweepTriangulation (graph); break;
  case 1: /*grDelaunayTriangulation (graph); */ break;
  case 2: grMinMaxAngleTriangulation (graph); break;
  case 3: grMaxMinHeightTriangulation (graph); break;
  case 4: grMinMaxSlopeTriangulation (graph); break;
  case 5: grIncrementalRegularTriangulation (graph); break;
  case 6: /* grDelaunayTriangulation (graph); */ break;
  case 7: /* grDelaunayTriangulation (graph); */ break;
  };

  if (! needsPlanesweep [i]) 
    grCleanConvexHull (graph); /* this won't get rid of every flat triangle 
				  since the algorithm assumes some nice 
                                  properties of the delaunay triangulation. */

  if (i == 7)
    write_vect_voronoi (parm.output->answer, grQE (graph), grSI (graph));
  else
    write_vect (parm.output->answer, grQE (graph), grSI (graph), (i == 6));
  grDispose (graph);

  return 0;
}
  
