/**** array.c ****/
/*
**  Written by David Gerdes  Fall 1992
**  US Army Construction Engineering Research Lab
**  Copyright  David Gerdes  USA-CERL  1992
*/


#include "stdio.h"
#include "Vect.h"
#include "array.h"

/*
**  Routines to provide easy interface to create and manage arrays.
**
**  
** Array_new_struct (size)
**      Create new token for array.
**
** Array_alloc (Array, num)
**      Allocate/reallocate array based on num requested
**
** Array_destroy (Array)
**      Delete array and structure associated w/ it
**
*/

char *malloc ();

static int array_chunk_size = 20;

Array_set_chunk_size (size)
    int size;
{
    array_chunk_size = size;
}

struct array_t *
Array_new_struct (size)
    int size;
{
    struct array_t *P;

    if (NULL == (P = (struct array_t *) malloc (sizeof (struct array_t))))
	return NULL;

    P->num = 0;
    P->size = size;
    P->n_alloced = 0;
    P->data = NULL;

    return P;
}

/* returns 0 or  -1 on out of memory */
Array_alloc (Array, num)
    struct array_t *Array;
    int num;
{
    char *P;
    int alloced;

    if (num <= Array->n_alloced)
      return 0;

    alloced = Array->n_alloced;
    P = (char *) dig__alloc_space (num, &alloced, array_chunk_size, 
	    (char *) Array->data, Array->size);
    if (P == NULL)
	return dig_out_of_memory ();

    Array->data = P;
    Array->n_alloced  = alloced;

    return 0;
}

Array_destroy (Array)
    struct array_t *Array;
{
    if (Array->n_alloced)
	free (Array->data);
    free (Array);

    return 0;
}
