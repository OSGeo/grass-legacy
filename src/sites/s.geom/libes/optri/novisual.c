
#include "internoptri.h"

/*---------------------------------------------------------------------------*/

static void
dummyAllVisual ()

{}

/*---------------------------------------------------------------------------*/

char *
noVisualGetFileName ()

{
  static char fname[50];

  printf ("Filename? "); fflush (stdout);
  scanf ("%s", fname);

  return fname;
}

/*---------------------------------------------------------------------------*/

static int
keybordGetButton (m, v, g)
     
     void *m;
     visualType *v;
     graphType *g;

{
  int theChoice;

  printf ("\n");
  printf ("\n");
  printf ("                       Triangulations\n\n");
  printf (" 1 ... Initial (Plane Sweep)         6 ... MaxMin Height (n^3)\n");
  printf (" 2 ... Delaunay (Lawson)             7 ... MinMax Slope (n^3)\n");
  printf (" 3 ... MinMax Angle (n^2 log n)      8 ... Delaunay (Incremental)\n");
  printf (" 4 ... MinMax Angle (n^3)            9 ... Regular (Incremental)\n");
  printf (" 5 ... MaxMin Height (n^2 log n)    10 ... Verify Delaunayness\n");
  printf ("11 ... Verify Regularity            12 ... Mark Duplicate Points\n");
  printf ("13 ... Save Triangulation           14 ... Save LONG\n");
  printf ("15 ... Remove Duplicate Points      16 ... Clean Convex Hull\n");
  printf ("17 ... Remove Duplicate Edges       18 ... Remove Overlap\n");
  printf ("0 ... QUIT\n");
  printf ("\n");
  printf ("Make a Choice:  "); (void) fflush(stdout);
  scanf ("%d", &theChoice);
  printf ("\n");

  printf("%d\n", theChoice);
  return theChoice;
}

/*---------------------------------------------------------------------------*/

menuType *
dummyMakeMainMenu ()

{
  return (void *) NULL;
}

/*---------------------------------------------------------------------------*/
/* define an instance "dummyVisual" using the dummy visual function */

visualType *
dummyVisual ()

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
