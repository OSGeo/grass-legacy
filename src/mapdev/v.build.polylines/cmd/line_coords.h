
#ifndef LINE_COORDSH
#define LINE_COORDSH

struct line_coords {
  double *x;
  double *y;
  int n_coords;
  int alloc_coords;
};


struct line_coords* init_line_coords ();
int reset_line_coords (struct line_coords*);
int resize_line_coords (struct line_coords*, int);
int free_line_coords (struct line_coords*);

#endif
