/***************************************************************/
/*                                                             */
/*     stash.h		     in    ~/src/Gdrain		       */
/*                                                             */
/*       This header file declares the global variables and    */
/*       the structures that are to be used for command        */
/*       line processing.                                      */
/*                                                             */
/***************************************************************/

#include <stdio.h>
#define      DRAIN_PATH_LAYER        1
#define      ELEVATION_LAYER         2
#define      START_PT                3

#include "point.h"
#ifdef MAIN

    struct variables 
    {
        char *alias;
        int position;
    } 

    variables [] = {
 
         {"output",DRAIN_PATH_LAYER},
         {"input",ELEVATION_LAYER},
         {"coor",START_PT}
    };

    char drain_path_layer[64];
    char elevation_layer[64];
    struct point  *head_start_pt = NULL;
 
#else 

    extern char drain_path_layer[];
    extern char elevation_layer[];
    extern struct point  *head_start_pt;

#endif

/****************END OF "GDRAIN_CMD_LINE.H"**********************/ 

