/**** cross.c ****/
/*
**  Written by David Gerdes  Fall 1992
**  US Army Construction Engineering Research Lab
**  Copyright  David Gerdes  USA-CERL  1992
*/


#include "cutter.h"
#include "math.h"

main (argc, argv)
    char *argv[];
{
  struct line_t L1;
  struct line_t L2;
  double x, y;
  int ret;

      L1.p1.x = atof (argv[1]);
      L1.p2.x = atof (argv[2]);
      L1.p1.y = atof (argv[3]); 
      L1.p2.y = atof (argv[4]);
      L2.p1.x = atof (argv[5]); 
      L2.p2.x = atof (argv[6]);
      L2.p1.y = atof (argv[7]); 
      L2.p2.y = atof (argv[8]);

      ret = intersect (L1, L2, &x,&y);

      printf (" return =  %2d: %s\n", ret, ret ? "Intersect" : "No Intersect");

      return !ret;
}
