
/* Includes macros etc */

#define GET_VAL 0
#define SET_VAL 1

typedef char d_type[256];
typedef d_type field_data[32];

typedef struct _site_array{
  double *x;
  double *y;
  int n_sites, alloc_sites;
} site_array;
