/**** array.h ****/
/*
**  Written by David Gerdes  Fall 1992
**  US Army Construction Engineering Research Lab
**  Copyright  David Gerdes  USA-CERL  1992
*/


/* array.h */


#ifndef VOID_T
#define VOID_T char 
#endif

/* copy this structure exactly, except for the type of 'data' */
struct array_t {
    VOID_T *data;
    int n_alloced;
    int num;
    int size;
};
