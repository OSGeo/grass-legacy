
#include "internoptri.h"

/*---------------------------------------------------------------------------*/

static void 
dummyAllVisual (void)

{}

/*---------------------------------------------------------------------------*/

char *
noVisualGetFileName (void)

{
  static char fname[50];

  fprintf (stdout,"Filename? "); fflush (stdout);
  scanf ("%s", fname);

  return fname;
}

/*---------------------------------------------------------------------------*/

static int 
keybordGetButton (void *m, visualType *v, graphType *g)

{
  int theChoice;

  fprintf (stdout,"\n");
  fprintf (stdout,"\n");
  fprintf (stdout,"                       Triangulations\n\n");
  fprintf (stdout," 1 ... Initial (Plane Sweep)         6 ... MaxMin Height (n^3)\n");
  fprintf (stdout," 2 ... Delaunay (Lawson)             7 ... MinMax Slope (n^3)\n");
  fprintf (stdout," 3 ... MinMax Angle (n^2 log n)      8 ... Delaunay (Incremental)\n");
  fprintf (stdout," 4 ... MinMax Angle (n^3)            9 ... Regular (Incremental)\n");
  fprintf (stdout," 5 ... MaxMin Height (n^2 log n)    10 ... Verify Delaunayness\n");
  fprintf (stdout,"11 ... Verify Regularity            12 ... Mark Duplicate Points\n");
  fprintf (stdout,"13 ... Save Triangulation           14 ... Save LONG\n");
  fprintf (stdout,"15 ... Remove Duplicate Points      16 ... Clean Convex Hull\n");
  fprintf (stdout,"17 ... Remove Duplicate Edges       18 ... Remove Overlap\n");
  fprintf (stdout,"0 ... QUIT\n");
  fprintf (stdout,"\n");
  fprintf (stdout,"Make a Choice:  "); (void) fflush(stdout);
  scanf ("%d", &theChoice);
  fprintf (stdout,"\n");

  fprintf (stdout,"%d\n", theChoice);
  return theChoice;
}

/*---------------------------------------------------------------------------*/

menuType *
dummyMakeMainMenu (void)

{
  return (void *) NULL;
}

/*---------------------------------------------------------------------------*/
/* define an instance "dummyVisual" using the dummy visual function */

visualType *
dummyVisual (void)

{
  visualType * tmp;

  tmp = MALLOC (visualType, 1);
 
  tmp->initialize = dummyAllVisual;
  tmp->drawEdge = dummyAllVisual;
  tmp->redrawEdge = dummyAllVisual;
  tmp->eraseEdge = dummyAllVisual;
  tmp->cleanUp = dummyAllVisual;
  tmp->redrawWindow = dummyAllVisual;
  tmp->makeMainMenu = dummyMakeMainMenu;
  tmp->makeChoiceMenu = dummyMakeMainMenu;
  tmp->resetChoice = dummyAllVisual;
  tmp->pause = dummyAllVisual;
  tmp->getChoiceButton = keybordGetButton;
  tmp->quality2dSetFunctions = dummyAllVisual;
  tmp->switchToPQE = dummyAllVisual;
  tmp->getFileName = noVisualGetFileName;
  tmp->winTitle = "";

  return tmp;
}

/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/
