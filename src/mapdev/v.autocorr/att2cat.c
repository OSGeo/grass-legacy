#include <stdlib.h>
#include "gis.h"
#include "Vect.h"
#include "autocorr.h"

double att2cat (struct Map_info *Map, int i)
{
  double atof (),tmp;
  extern struct Categories cats;

  tmp = atof (G_get_cat (i, &cats));
  if (!tmp)
    G_warning ("non-numeric or zero category label found");
  return tmp;
}

/*
 * char * G_get_cat (num, pcats) CELL num                     category number
 * struct Categories *pcats     structure to hold category info
 * 
 * Returns pointer to a string describing category.
 * 
 * <3:11am> /usr/grass4/src/libes/gis# more cats.c
 */
