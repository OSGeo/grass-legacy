/* %W% %G% */
/***************************************************************/
/*                                                             */
/*       stash.h     in    ~/src/Gsurface                      */
/*                                                             */
/*       This header file declares the global variables and    */
/*       the structures that are to be used for command        */
/*       line processing.                                      */
/*                                                             */
/***************************************************************/

#include <stdio.h>
#define      INPUT_LAYER	1
#define      OUTPUT_LAYER	2

#ifdef MAIN

    struct variables 
    {
        char *alias;
        int position;
    } 

    variables [] = {
 
         "input",INPUT_LAYER,
         "output",OUTPUT_LAYER
    };

    static int n_variables = 2;

    char input_layer[64];
    char output_layer[64];
 
#else 

    extern char input_layer[];
    extern char output_layer[];

#endif

/**********************END OF "STASH.H"*************************/ 



