/*
 *   Darea
 *
 *   Usage:  Darea [-fill] vector-file-name [color]
 *
 */

#include "gis.h"
struct Cell_head window ;

main(argc, argv)
    int argc ;
    char **argv ;
{
    char window_name[64] ;
    int t, b, l, r ;
    char *mapset;
    char name[20];
    char color[8];
    int fill;
    static char me[6] = {"Darea"};
    static char usage[36] = {"  [-f]   vector-file-name   [color]"};
    int length;
    static int cflag = {0};
    char gisbase[256];
    char command[512];
    char Dvect_color[8];
    int where_am_i();

/* Exit if the number of arguments are incorrect */
    if (argc != 2 && argc != 3 && argc != 4)
     {
      D_usage(me,usage);
      exit(-1);
     }

/* Set variables: "name", "color", and "fill" */
    if (argc == 2)
     {
/* Check the length of the "name" to ensure it is not too long */
      length = strlen(argv[1]);
      if (length > 19)
       {
fprintf(stderr,"\nFile name:  '%s' is LONGER than the allowable 19 characters.\n\n",
        argv[1]);
        D_usage(me,usage);
        exit(-1);
       }
      strcpy(name,argv[1]);
/* default color is red */
      strcpy(color,"red");
/* default is NO fill */      
      fill = 0;
     } 
    else
      if (argc == 3)
       {
        if ( (strcmp(argv[1],"-f")==0)    ||   (strcmp(argv[1],"-F")==0)     || 
             (strcmp(argv[1],"-fill")==0) ||   (strcmp(argv[1],"-Fill")==0)  || 
             (strcmp(argv[1],"-FILL")==0) ||   (strcmp(argv[1],"f")==0)      || 
             (strcmp(argv[1],"F")==0)     ||   (strcmp(argv[1],"fill")==0)   || 
             (strcmp(argv[1],"Fill")==0)  ||   (strcmp(argv[1],"FILL")==0)    ) 
         {
          fill = 1;
/* Check the length of the "name" to ensure it is not too long */
          length = strlen(argv[2]);
          if (length > 19)
           {
fprintf(stderr,"\nFile name:  '%s' is LONGER than the allowable 19 characters.\n\n",
            argv[2]);
            D_usage(me,usage);
            exit(-1);
           }
          strcpy(name,argv[2]);
/* default color is red */
          strcpy(color,"red");
         }
        else
         {
/* default is NO fill */      
          fill = 0;
/* Check the length of the "name" to ensure it is not too long */
          length = strlen(argv[1]);
          if (length > 19)
           {
fprintf(stderr,"\nFile name:  '%s' is LONGER than the allowable 19 characters.\n\n",
            argv[1]);
            D_usage(me,usage);
            exit(-1);
           }
          strcpy(name,argv[1]);
/* Check the length of the "color" to ensure it is not too long */
          length = strlen(argv[2]);
          if (length > 7)
           {
fprintf(stderr,"\nColor:  '%s' is LONGER than the allowable 7 characters.\n\n",
            argv[2]);
            cflag = 1;
            strcpy(argv[2],"red");
           }
          strcpy(color,argv[2]);
         }
       }
      else
        if (argc == 4)
         {
          if ( (strcmp(argv[1],"-f")==0)    ||  (strcmp(argv[1],"-F")==0)     ||
               (strcmp(argv[1],"-fill")==0) ||  (strcmp(argv[1],"-Fill")==0)  ||
               (strcmp(argv[1],"-FILL")==0) ||  (strcmp(argv[1],"f")==0)      ||
               (strcmp(argv[1],"F")==0)     ||   (strcmp(argv[1],"fill")==0)  ||
               (strcmp(argv[1],"Fill")==0)  ||   (strcmp(argv[1],"FILL")==0)   )
           {
            fill = 1;
/* Check the length of the "name" to ensure it is not too long */
            length = strlen(argv[2]);
            if (length > 19)
             {
fprintf(stderr,"\nFile name:  '%s' is LONGER than the allowable 19 characters.\n\n",
              argv[2]);
              D_usage(me,usage);
              exit(-1);
             }
            strcpy(name,argv[2]);
/* Check the length of the "color" to ensure it is not too long */
            length = strlen(argv[3]);
            if (length > 7)
             {
fprintf(stderr,"\nColor:  '%s' is LONGER than the allowable 7 characters.\n\n",
              argv[3]);
              cflag = 1;
              strcpy(argv[3],"red");
             }
            strcpy(color,argv[3]);
           }
          else
           {
/* default is NO fill */      
            fill = 0;
/* Check the length of the "name" to ensure it is not too long */
            length = strlen(argv[2]);
            if (length > 19)
             {
fprintf(stderr,"\nFile name:  '%s' is LONGER than the allowable 19 characters.\n\n",
              argv[2]);
              D_usage(me,usage);
              exit(-1);
             }
             strcpy(name,argv[2]);
/* Check the length of the "color" to ensure it is not too long */
             length = strlen(argv[3]);
             if (length > 7)
              {
fprintf(stderr,"\nColor:  '%s' is LONGER than the allowable 7 characters.\n\n",
               argv[3]);
               cflag = 1;
               strcpy(argv[3],"red");
              }
            strcpy(color,argv[3]);
           }
         }

/* Initialize the GIS calls */
    G_gisinit(argv[0]) ;

/* Determine the "mapset" based on your order of mapsets */
    if ( ! (mapset = G_find_vector2(name,"")) )
     {
      fprintf(stderr,"\nVector file name:  '%s' NOT found.\n\n",name);
      D_usage(me,usage);
      exit(-1);
     }
      
    if (cflag)
     {
      fprintf(stderr,"\nThe default color 'red' will be used.\n\n");
     }

/* Check if color is "white" and fill option is "off" */
    if ( (strcmp(color,"white")==0) && (fill!=1) )
      strcpy(Dvect_color,"red");
    else
      strcpy(Dvect_color,"white");

/* Perform "Dvect" for vector file "name" */
    strcpy(gisbase,G_gisbase() );
    sprintf(command,"%s/bin/Dvect %s %s",gisbase,name,Dvect_color);
    system(command);

    R_open_driver();

    if (D_get_cur_wind(window_name))
	G_fatal_error("No current graphics window") ;

    if (D_set_cur_wind(window_name))
	G_fatal_error("Current graphics window not available") ;

/* Read in the map window associated with window */
    G_get_window(&window) ;

    if (D_check_map_window(&window))
	G_fatal_error("Setting map window") ;

    if (G_set_window(&window) == -1) 
	G_fatal_error("Current graphics window not settable") ;

/* Determine conversion factors */
    if (D_get_screen_window(&t, &b, &l, &r))
	G_fatal_error("Getting screen window") ;
    if (D_do_conversions(&window, t, b, l, r))
	G_fatal_error("Error in calculating conversions") ;
    
    where_am_i(name,mapset,color,fill,Dvect_color) ;

    fprintf(stderr,"\n") ;

    R_close_driver();

    exit(1);
}
