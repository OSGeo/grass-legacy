#include <math.h>
#include "internoptri.h"
#include "bitvector.h"
#include "geom/lia.h"
#include "geom/sos.h"

/*--------------------------------------------------------------------------*/

#define SIMAGIC 2343452

/*--------------------------------------------------------------------------*/

typedef struct {

  coordType *x;
  coordType *y;
  coordType *z;
  bvType *dupVect;
  indexType max, ns, nofDuplicates;
  coordType xMin, xMax, yMin, yMax, zMin, zMax;
  int useLIA, nofDeci;
  int magic;

} siteType;

/*--------------------------------------------------------------------------*/

#ifdef TESTMAGIC

  static siteType *
siMagic (void *s)

  {
    void (*X) ();

    if (((siteType *) s)->magic != SIMAGIC) {
      fprintf (stdout,"ERROR: siMagic: wrong magic.\n");
      X();
    }

    return (siteType *) s;
  }

#else

#define siMagic(s) ((siteType *) s)

#endif


#define S (siMagic (s))

int 
siIsSItype (void *s)

{
  siMagic (s);
  return 1;
}


/*--------------------------------------------------------------------------*/

#define LIA_MAX_LENGTH 16

/*--------------------------------------------------------------------------*/

static void siSosInitialize (int nofVertices, int nofDeci)

{
  static int firstTime = 1;
  double tenPow;
  int i;

  if (firstTime) {

    tenPow = 1;
    for (i = 0; i < nofDeci; i++)
      tenPow *= 10;

    firstTime = 0;
    lia_maximum (LIA_MAX_LENGTH);
    sos_init (nofVertices, 5, (double) (((double) 1.0) / tenPow), 
	      LIA_MAX_LENGTH, 8);
    lia_stack_limit (200);
  } else {
    fprintf (stdout,"ERROR: siSosInitialize: can't use two long integer objects.\n");
    exit (1);
  }
}

/*-------------------------------------------------------------------------*/

#define liaMAXDIGITS ((int) 15)

static void 
siSosLoadPoint (int i, int a, double x, double y, double w)

{
  Lia_ptr tmp;

  if (i < 0) {
    fprintf (stdout,"ERROR:siSosLoadPoint\n");
    exit (1);
  }

  lia_ffpload (tmp = lia_pushf (LIA_NULL), liaMAXDIGITS, a, w);
  sos_param (i + 1, (int) 3, tmp);
  lia_negtop ();

  lia_ffpload (tmp = lia_pushf (LIA_NULL), liaMAXDIGITS, a, x);
  sos_param (i + 1, (int) 1, tmp);
  lia_pushtop (); lia_times ();

  lia_ffpload (tmp = lia_pushf (LIA_NULL), liaMAXDIGITS, a, y);
  sos_param (i + 1, (int) 2, tmp);
  lia_pushtop (); lia_times ();

  lia_plus (); lia_pushtop ();
  sos_param (i + 1, (int) 4, lia_popf ()); /* sqr(x) + sqr(y) */

  lia_plus ();  
  sos_param (i + 1, (int) 5, lia_popf ()); /* sqr(x) + sqr(y) - w */
}

/*-------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

static double 
truncatedDouble (double d, int nofDeci)

{
  double tmp;

  tmp = pow ((double) 10, (double) nofDeci);
  return floor (d * tmp) / tmp;
}

/*--------------------------------------------------------------------------*/

void *
siNewAdvanced (n, nofSetPoints, useLIA, nofDeci, x, y, z)

     indexType n;
     indexType nofSetPoints;
     int useLIA;
     int nofDeci;
     coordType *x, *y, *z;

{
  siteType *tmp;
  int i;

  tmp = MALLOC (siteType, 1);
  tmp->magic = SIMAGIC;
  tmp->max = n;
  tmp->nofDuplicates = 0;
  tmp->ns = nofSetPoints;

  if ((x == NULL) && (tmp->max > 0))
    tmp->x = MALLOC (coordType, tmp->max);
  else 
    tmp->x = x;

  if ((y == NULL) && (tmp->max > 0))
    tmp->y = MALLOC (coordType, tmp->max);
  else 
    tmp->y = y;

  if ((x == NULL) && (y == NULL) && (z == NULL) && (tmp->max > 0))
    tmp->z = MALLOC (coordType, tmp->max);
  else 
    tmp->z = z;

  tmp->nofDeci = nofDeci;

  if (tmp->useLIA = useLIA) {
    siSosInitialize (tmp->max, nofDeci);
  }

  if (tmp->ns > 0) {
    tmp->xMax = tmp->xMin = tmp->x[0];
    tmp->yMax = tmp->yMin = tmp->y[0];
    if (tmp->z != NULL) {
      tmp->zMax = tmp->zMin = tmp->z[0];
      if (tmp->useLIA)
	siSosLoadPoint (1, tmp->nofDeci, tmp->x[0], tmp->y[0], tmp->z[0]);
    } else 
      if (tmp->useLIA)
        siSosLoadPoint (1, tmp->nofDeci, tmp->x[0], tmp->y[0], (coordType) 0);

    for (i = 1; i < tmp->ns; i++) {
      tmp->xMax = (tmp->xMax > tmp->x[i] ? tmp->xMax : tmp->x[i]);
      tmp->xMin = (tmp->xMin < tmp->x[i] ? tmp->xMin : tmp->x[i]);
      tmp->yMax = (tmp->yMax > tmp->y[i] ? tmp->yMax : tmp->y[i]);
      tmp->yMin = (tmp->yMin < tmp->y[i] ? tmp->yMin : tmp->y[i]);
      if (tmp->z != NULL) {
	tmp->zMax = (tmp->zMax > tmp->z[i] ? tmp->zMax : tmp->z[i]);
	tmp->zMin = (tmp->zMin < tmp->z[i] ? tmp->zMin : tmp->z[i]);
	if (tmp->useLIA)
	  siSosLoadPoint (i, tmp->nofDeci, (double) tmp->x[i], 
			  (double) tmp->y[i], (double) tmp->z[i]);
      } else
	if (tmp->useLIA)
          siSosLoadPoint (i, tmp->nofDeci, (double) tmp->x[i], 
			  (double) tmp->y[i], (double) 0);
    }
  }
    
  tmp->dupVect = bvNew (n, BV_FIXED_SIZE);
  bvReset (tmp->dupVect);

  return (void *) tmp;
}

/*--------------------------------------------------------------------------*/

void *
siNew (n, nofDeci)

     indexType n;
     int nofDeci;

{
  return siNewAdvanced (n, (indexType) 0, 1, nofDeci, (coordType *) NULL, 
			(coordType *) NULL, (coordType *) NULL);
}

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

indexType
siNS (s)

     void *s;

{
  return S->ns;
}

/*--------------------------------------------------------------------------*/

void 
siNSset (void *s, int nNew)

{
  S->ns = nNew;
}

/*--------------------------------------------------------------------------*/

indexType
siNSmax (s)

     void *s;

{
  return S->max;
}

/*--------------------------------------------------------------------------*/

indexType
siNUS (s)

     void *s;

{
  return S->ns - S->nofDuplicates;
}

/*--------------------------------------------------------------------------*/

coordType
siSITEX (s, site)

     void *s;
     indexType site;

{
  if (site >= S->ns) {
    fprintf (stdout,"ERROR: SITEX: site out of bounds.\n");
    exit (1);
  }

  return S->x[site];
}
/*--------------------------------------------------------------------------*/

coordType
siSITEY (s, site)

     void *s;
     indexType site;

{
  if (site >= S->max) {
    fprintf (stdout,"ERROR: SITEY: site out of bounds.\n");
    exit (1);
  }

  return S->y[site];
}

/*--------------------------------------------------------------------------*/

coordType
siSITEZ (s, site)

     void *s;
     indexType site;

{
  if (site >= S->max) {
    fprintf (stdout,"ERROR: SITEZ: site out of bounds.\n");
    exit (1);
  }

  if (S->z == NULL)
    return (coordType) 0;
  else
    return S->z[site];
}

/*--------------------------------------------------------------------------*/

coordType 
siMaxX (void *s)

{
  return S->xMax;
}

/*--------------------------------------------------------------------------*/

coordType 
siMinX (void *s)

{
  return S->xMin;
}

/*--------------------------------------------------------------------------*/

coordType 
siMaxY (void *s)

{
  return S->yMax;
}

/*--------------------------------------------------------------------------*/

coordType 
siMinY (void *s)

{
  return S->yMin;
}

/*--------------------------------------------------------------------------*/

coordType 
siMaxZ (void *s)

{
  return S->zMax;
}

/*--------------------------------------------------------------------------*/

coordType 
siMinZ (void *s)

{
  return S->zMin;
}

/*--------------------------------------------------------------------------*/

int siIsDuplicateSite (s, site)

     void *s;
     indexType site;

{
  return (bvTest (S->dupVect, (int) site) != 0);
}

/*--------------------------------------------------------------------------*/

void
siMarkAsDuplicateSite (s, site)

     void *s;
     indexType site;

{
  if (! siIsDuplicateSite (s, site)) {
    bvSet (S->dupVect, (int) site);
    (S->nofDuplicates)++;
  }
}

/*--------------------------------------------------------------------------*/

void
siMarkAsNonDuplicateSite (s, site)

     void *s;
     indexType site;

{
  if (siIsDuplicateSite (s, site)) {
    bvClear (S->dupVect, (int) site);
    (S->nofDuplicates)--;
  }
}

/*--------------------------------------------------------------------------*/

void
siSITEreload (s, site, x, y, z)

     void *s;
     indexType site;
     coordType x, y, z;

{
  if (site >= S->max) {
    fprintf (stdout,"ERROR: SITEreload: site out of bounds.\n");
    exit (1);
  }

  S->x[site] = x = truncatedDouble (x, S->nofDeci);
  S->y[site] = y = truncatedDouble (y, S->nofDeci);
  if (S->z != NULL) {
    S->z[site] = z = truncatedDouble (z, S->nofDeci);
    if (S->useLIA)
      siSosLoadPoint (site, S->nofDeci, (double) x, (double) y, (double) z);
  } else {
    z = 0;
    if (S->useLIA)
      siSosLoadPoint (site, S->nofDeci, (double) x, (double) y, 
		      (double) 0);
  }

  if (S->ns > 0) {
    S->xMax = (S->xMax > x ? S->xMax : x);
    S->xMin = (S->xMin < x ? S->xMin : x);
    S->yMax = (S->yMax > y ? S->yMax : y);
    S->yMin = (S->yMin < y ? S->yMin : y);
    if (S->z != NULL) {
      S->zMax = (S->zMax > z ? S->zMax : z);
      S->zMin = (S->zMin < z ? S->zMin : z);
    }
  } else {
    S->xMax = x;
    S->xMin = x;
    S->yMax = y;
    S->yMin = y;
    if (S->z != NULL) {
      S->zMax = z;
      S->zMin = z;
    }
  }
}

/*--------------------------------------------------------------------------*/

indexType
siSITEload (s, x, y, z)

     void *s;
     coordType x, y, z;

{
  if (S->ns < S->max) {
    siMarkAsNonDuplicateSite (s, S->ns);
    siSITEreload (s, S->ns, x, y, z);
    (S->ns)++;
  } else {
    fprintf (stdout,"ERROR: SITEload: not enough space.\n");
    exit (1);
  }

  return S->ns - 1;
}
  
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

static siteType *T;

static int 
siDoubleGT (void *i1, void *i2)

{
  coordType x1, x2, y1, y2;

  x1 = siSITEX (T, *((indexType *) i1));
  x2 = siSITEX (T, *((indexType *) i2));

  if (x1 != x2) return (x1 > x2 ? 1 : -1);

  y1 = siSITEY (T, *((indexType *) i1));
  y2 = siSITEY (T, *((indexType *) i2));

  if (y1 != y2) return (y1 > y2 ? 1 : -1);

  return 0;
}

/*--------------------------------------------------------------------------*/

#define DET21(i,j,a) sos_minor2 ((int) i + 1, (int) j + 1, (int) a, (int) 0)

static int 
siSosGT (void *i1, void *i2)

{
  int sosIndex1, sosIndex2;
  int result;
  
  sosIndex1 = *((indexType *) i1);
  sosIndex2 = *((indexType *) i2);

  if (result = lia_sign (DET21 (sosIndex1, sosIndex2, 1)))
    return result;
  else
    return lia_sign (DET21 (sosIndex1, sosIndex2, 2));
}

/*--------------------------------------------------------------------------*/

int 
siMarkDuplicateSites (void *s)

{
  int min, i, j, count;
  indexType * Index;

  Index = MALLOC (indexType, siNS (s));

  T = S;

  for (i = 0; i < siNS (s); i++) {
    siMarkAsNonDuplicateSite (s, i);
    Index [i] = i;
  }

  qsort (Index, siNS (s), sizeof (indexType), 
	 (S->useLIA ? siSosGT : siDoubleGT));

  count = 0;

  for (i = 0; i < siNS (s) - 1;) {
    j = i + 1;
    min = i;

    while ((j < siNS (s)) && 
	   ((siSITEX (s, Index[i]) == siSITEX (s, Index[j])) &&
	    (siSITEY (s, Index[i]) == siSITEY (s, Index[j])))) {
      min = (Index[min] > Index[j] ? j : min);
      j++;
    }

    if (j > i + 1) {
      count += j - i - 1;
      while (i < j) {
	if (i != min) 
	  siMarkAsDuplicateSite (s, Index[i]);
	i++;
      }
    } else
      i = j;
  }

/*  for (i = 0; i < siNS (s); i++)
    fprintf (stdout,"%d %f %f %d\n", Index[i],
	    siSITEX (s, Index[i]), siSITEY (s, Index[i]),
	    siIsDuplicateSite (s, Index[i]));*/

  fprintf (stdout,"siMarkDeletedDuplicates: %d duplicate sites marked.\n\n", 
	  count);
  
  if (count && S->useLIA) {
    printf 
    ("NOTE: long integer coordinates used for comparison. due to roundoff\n");
    printf 
    ("      two sites may have identical coordinates, although in the\n");
    fprintf (stdout,"      input data they don't.\n");
  }

  FREE (Index);

  S->nofDuplicates = count;

  return count;
}  

/*--------------------------------------------------------------------------*/

int
siRemoveDuplicateSitesExpert (s, Index)

     void *s;
     indexType *Index;

{
  int front, back, nofDup;
  
  nofDup = siMarkDuplicateSites (s);

  if (nofDup > 0) {
    back = 0;
    while (! siIsDuplicateSite (s, back)) {
      if (Index) Index [back] = back;
      back++;
    }

    if (Index) Index [back] = -1;

    front = back + 1;
    while (siIsDuplicateSite (s, front)) {
      if (Index) Index [front] = -1;
      front++;
    }

    while (front < siNS (s)) {
      if (Index) Index [front] = back;
      siSITEreload (s, back, siSITEX (s, front), siSITEY (s, front),
		    siSITEZ (s, front));
      siMarkAsNonDuplicateSite (s, back);
      
      back++;
      front++;
      while ((front < siNS (s)) && (siIsDuplicateSite (s, front))) {
	if (Index) Index [front] = -1;
	front++;
      }
    }

    while (back < siNS (s)) {
      siMarkAsNonDuplicateSite (s, back);
      back++;
    }

    S->ns = S->ns - nofDup;
    S->nofDuplicates = 0;
  }

  fprintf (stdout,"siRemoveDuplicates: %d duplicate sites removed.\n\n", nofDup);
  return nofDup;
}

/*--------------------------------------------------------------------------*/

int 
siRemoveDuplicateSites (void *s)

{
  return siRemoveDuplicateSitesExpert (s, NULL);
}

/*--------------------------------------------------------------------------*/

void 
siPrintSites (void *s, FILE *fp)
{
  int i;

  fprintf (fp, "# site x y z,   site indices go from 0 to %d\n",siNS (s) - 1);

  for (i=0; i < siNS (s); i++) 
    fprintf (fp, "site %f %f %f\n", siSITEX (s, i), siSITEY (s, i), 
	                               siSITEZ (s, i));
  
  fprintf (fp, "\n");
}

/*--------------------------------------------------------------------------*/

static char *
siReadLine (FILE *fp)

{
  static char buf [80];
  char * tmp;
  int i;

  for (i = 0; i < 80; i++) buf [i] = ' ';

  while (fgets (buf, 80, fp) != NULL) {

    tmp = buf;
    if (*tmp == '#') 
      continue;

    while (isspace (*tmp))
      tmp++;

    return tmp;

  }

  return NULL;
}

/*------------------------------------------------------------------------*/

static int 
siCountB (char *buf, int bufLen)

{
  int i, isNegative;

  i = 0;

  if (isNegative = (buf[i] == '-')) i++;
  if (buf[i] == '0') return 0;
  while ((buf[i] != '\n') && (buf[i] != ' ') && (buf[i] != '.')) i++;

  if (isNegative)
    return i - 1;
  else
    return i;
}

/*------------------------------------------------------------------------*/

static int 
siCountA (char *buf, int bufLen)

{
  int i, firstNon0;

  i = bufLen - 1;

  while ((i >= 0) && (buf[i] != '.') && (buf[i] == '0')) i--;

  if ((i < 0) || (buf[i] == '.')) return 0;
  
  firstNon0 = i;
    
  while ((i >= 0) && (buf[i] != '.')) i--;

  if (i < 0) return 0;

  return firstNon0 - i;
}

/*--------------------------------------------------------------------------*/

static void siUpdateMax (int *maxW, int *maxA, char *buf, int nofNums)

{
  int i, j, n;

  i = 0;
  
  while (nofNums) {
    while (buf[i] == ' ') i++;
    j = i;
    while ((buf[j] != '\n') && (buf[j] != ' ')) j++;
    
    n = siCountB (&(buf[i]), j - i);
    *maxW = (*maxW > n ? *maxW : n);
    n = siCountA (&(buf[i]), j - i);
    *maxA = (*maxA > n ? *maxA : n);

    i = j;
    nofNums--;
  }
}

/*--------------------------------------------------------------------------*/

static int 
siDoReadSites (void *s, FILE *fp, int examine, int *nofSites, int *maxW, int *maxA, int *dataIs2d, int *dataIs3d)

{
  static char * inputFormat [4] = {"site %lf %lf %lf %lf", "%lf %lf %lf %lf", 
				   "site %lf %lf %lf", "%lf %lf %lf"};
  static int inputDimension[4] = {3, 3, 2, 2};
  static int stringShift[4] = {4, 0, 4, 0};

  int dataDimension[4], i;
  double x, y, z;
  char * line;

  dataDimension[2] = dataDimension[3] = 0;
  *nofSites = 0;

  if (examine) *maxW = *maxA = 0;

  while ((line = siReadLine (fp)) != NULL) 
    
    for (i = 0; i <= 3; i++)
      if (sscanf (line, inputFormat[i], &x, &y, &z) == inputDimension[i]) {
	if (examine) {
	  (*nofSites)++;
	  dataDimension [inputDimension [i]] = 1;
	  siUpdateMax (maxW, maxA, line + stringShift [inputDimension [i]], 
		       inputDimension [i]);
	} else {
	  if (inputDimension [i] == 3)
	    siSITEload (s, (coordType) x, (coordType) y, (coordType) z);
	  else
	    siSITEload (s, (coordType) x, (coordType) y, (coordType) 0);
	}
	break;
      }

  *dataIs2d = dataDimension[2];
  *dataIs3d = dataDimension[3];

  return 0;
}

/*--------------------------------------------------------------------------*/

void 
siReadSites (char *fname, void *s)

{
  int dummy1, dummy2, dummy3, dummy4, dummy5;
  FILE *fp;

  if ((fp = fopen (fname, "r")) == NULL) {
    fprintf (stdout, 
	     "ERROR: siReadSites:  can't open \"%s\" to read.\n", fname);
    exit (1);
  }

  siDoReadSites (s, fp, (int) 0, &dummy1, &dummy2, &dummy3, &dummy4, &dummy5);

  if (fclose (fp) == EOF)
    fprintf (stdout, "ERROR: siReadSites: could not close file \"%s\".\n",fname);
}

/*--------------------------------------------------------------------------*/

void 
siExamineSites (char *fname, int *nofSites, int *maxW, int *maxA)

{
  FILE *fp;
  int dataIs2d, dataIs3d;

  if ((fp = fopen (fname, "r")) == NULL) {
    fprintf (stdout, 
	     "ERROR: siReadSites:  can't open \"%s\" to read.\n", fname);
    exit (1);
  }

  siDoReadSites ((void *) NULL, fp, (int) 1, nofSites, maxW, maxA,
		 &dataIs2d, &dataIs3d);

  if (dataIs2d && dataIs3d)
    fprintf (stdout,"\nWarning: Input Data: number of coordinates varies\n\n");
  if (dataIs2d && (! dataIs3d))
    printf 
      ("\nInput data is two dimensional; z-coordinates initialized to 0.\n");
  if (dataIs3d && (! dataIs2d))
    fprintf (stdout,"\nInput data is three dimensional.\n");

  if (fclose (fp) == EOF)
    fprintf (stdout, "ERROR: siExamineSites: could not close file \"%s\".\n",fname);

}

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
