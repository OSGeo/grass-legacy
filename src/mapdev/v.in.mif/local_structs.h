#ifndef LOCAL_STRUCTS_H_
#define LOCAL_STRUCTS_H_

/* Includes macros etc */

#include "gis.h"
#include "Vect.h"

#define GET_VAL 0
#define SET_VAL 1

typedef char d_type[256];
typedef d_type field_data[32];

typedef struct _site_array{
  double *x;
  double *y;
  int n_sites, alloc_sites;
} site_array;


typedef struct _line_array{
  struct line_pnts *gplines;
  int n_glines, alloc_glines;
} line_array;


typedef struct _type_array {
  int n_chars;
  int alloc_chars;
  char *list;
  int *entities;
} type_array;

#endif /* LOCAL_STRUCTS_H_ */
