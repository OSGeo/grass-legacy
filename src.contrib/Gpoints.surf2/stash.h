/* %W% %G% */
/***************************************************************/
/*                                                             */
/*       stash.h     in    ~/src/Ginterp		       */
/*                                                             */
/*       This header file declares the global variables and    */
/*       the structures that are to be used for command        */
/*       line processing.                                      */
/*                                                             */
/***************************************************************/

#include <stdio.h>
#define      INPUT	1
#define      OUTPUT	2
#define      GOAL	3

#ifdef MAIN

    struct Command_keys keys[]=
    {
        {"input",INPUT},
        {"output",OUTPUT},
        {"points",GOAL},
        {NULL,0}
    };

    char 	input[64];
    char 	output[64];
    int		goal = 12;	/* 12 nearest neighbors unless user specified */
 
#else 

    extern char input[];
    extern char output[];
    extern int	goal;

#endif

/**********************END OF "STASH.H"*************************/ 



