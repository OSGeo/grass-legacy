/*****************************************************************************/
/*                                                                           */
/*   d.distance                                                              */
/*                                                                           */
/*   Usage:  d.distance vector-file-name  [-overlay]                         */
/*                                                                           */
/*****************************************************************************

                                    main
                                      |
                                      |
                                 a_n_distance
                                      |
                                      | 
                                      F------->z(END)
                                      |
                    __________________|__________________    
                    |                                   |
                    |                                   |
                    A----->z(END)                       I
                    |                                   |
                    |                         __________|________
             Z<---->B                         |                 | 
                    |            z(END)<------U<--------------->T
             Z<---->C                         |                  
                    |                _________|__________________
      ______________|_______         |                          |
      |                    |         |                          |
      |                    |         a(START)      z(END)<------V----->a(START)
      L<-->Z               G<-->Z                               |      
      |        Z           |                       z(END)<------R----->a(START)
      |        |           |                                    |
      |------->D<----------|                                    e
               | 
               |
  Z<--->H------|------------------------->J<----->Z
        |                                 |
        |                   Z             |
        |                   |             |
        |------------------>O<------------|
                            |
                            |
      Z<---->E--------------|--------------------------->P<--->Z
             |                                           |
      Z      |                          Z                |
      |      |                          |                |
      M<-----|------------------------->N<---------------|
      |                                 |
      |                                 |
      |-------------------->Y<----------|
                            |                
                            |               
           _________________|_______________________________
           |                                               |
           |                                               |
           |                                      _________|___________
           |                                      |                   |
           |                                      |                   |
           X                                  a(START)             z(END)
           |
           |
  z(END)<--K--->a(START)
           |
           |
           e
                                                             | 
                                               a(START)<-----Z----->z(END)


 *****************************************************************************/
/* "indicator" for PRESENT, INITIAL, TERMINAL, and LAST_M_PT structures are  */
/* defined as follows:                                                       */
/*                                                                           */
/* Indicator | Definition                                                    */
/*           |                                                               */
/*     1     | I & N (N1)                                                    */
/*     2     | I & N (N2)                                                    */
/*     3     | I & B                                                         */
/*     4     | I & E                                                         */
/*     5     | I & M                                                         */
/*           |                                                               */
/*     6     | T & N (N1)                                                    */
/*     7     | T & N (N2)                                                    */
/*     8     | T & B                                                         */
/*     9     | T & E                                                         */
/*     0     | T & M                                                         */
/*                                                                           */
/* NOTE: I is initial point (first point for measured line)                  */
/*       N is node point                                                     */
/*       B is beginning point of a segment                                   */
/*       E is ending point of a segment                                      */
/*       M is middle point of a segment and is not either of the end points  */
/*       T is terminal point (last point for measured line)                  */
/*       N1 is the beginning node number for an arc                          */
/*       N2 is the ending node number for an arc                             */
/*****************************************************************************/
/*  GLOBAL VARIABLES                                                         */
#include "distance.h"
/*****************************************************************************/
main(argc, argv)
  int argc ;
  char **argv ;
 {
  extern int a_n_distance();
  int t, b, l, r ;
  char *mapset;
  char name[20];
  static char prog_name[15];
  static char usage[33] = {"  vector-file-name    [-overlay]"};
  int length;
  char gisbase[256];
  char command[512];
  struct Map_info *map;
  struct line_pnts *p;
  int overlay_flag;

#ifdef DEBUG
fprintf(stderr,"main\n");
#endif DEBUG
/* Exit if the number of arguments are incorrect */
  if (argc != 2 && argc != 3)
   {
    strcpy(prog_name,argv[0]);
    D_usage(prog_name,usage);
    exit(0);
   }
/* Set variables: "name" */
/* Check the length of the "name" to ensure it is not too long */
  length = strlen(argv[1]);
  if (length > 19)
   {
fprintf(stderr,"\nFile name:  '%s' is LONGER than the allowable 19 characters.\n\n",
    argv[1]);
    strcpy(prog_name,argv[0]);
    D_usage(prog_name,usage);
    exit(0);
   }
  strcpy(name,argv[1]);
  if (argc==2)
   {
    overlay_flag = 0;
   }
  else
   {
    if ((strcmp(argv[2],"-o")==0)      ||(strcmp(argv[2],"-O")==0)      || 
        (strcmp(argv[2],"-overlay")==0)||(strcmp(argv[2],"-Overlay")==0)|| 
        (strcmp(argv[2],"-OVERLAY")==0)||(strcmp(argv[2],"-over")==0)   || 
        (strcmp(argv[2],"-OVER")==0)   ||(strcmp(argv[2],"-Over")==0)   || 
        (strcmp(argv[2],"o")==0)       ||(strcmp(argv[2],"O")==0)       || 
        (strcmp(argv[2],"overlay")==0) ||(strcmp(argv[2],"Overlay")==0) || 
        (strcmp(argv[2],"OVERLAY")==0) ||(strcmp(argv[2],"over")==0)    || 
        (strcmp(argv[2],"OVER")==0)    ||(strcmp(argv[2],"Over")==0)      )
     {
      overlay_flag = 1;
     }
   }
/* initialize structure pointers */
  map = (struct Map_info *)  malloc (sizeof(struct Map_info));
  if (map == NULL)
   {
    fprintf(stderr,"OUT OF MEMORY using malloc for \"map\" pointer.\n");
    exit(0);
   }
  p = (struct line_pnts *) malloc (sizeof(struct line_pnts));
  if (p == NULL)
   {
    fprintf(stderr,"OUT OF MEMORY using malloc for \"p\" pointer.\n");
    exit(0);
   }
/* Initialize the GIS calls */
  G_gisinit(argv[0]) ;
/* Determine the "mapset" based on your order of mapsets */
  if ( ! (mapset = G_find_vector2(name,"")) )
   {
    fprintf(stderr,"\nVector file name:  '%s' NOT found.\n\n",name);
    strcpy(prog_name,argv[0]);
    D_usage(prog_name,usage);
    exit(0);
   }
  exit ( a_n_distance(map,p,mapset,name,overlay_flag) );
 }
