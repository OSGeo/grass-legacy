#include <stdlib.h>
#include "gis.h"
#include "Vect.h"

/* Init int_list */
int 
dig_init_list ( struct ilist *list ) 
{
    list->value = NULL;
    list->n_values = 0;
    list->alloc_values = 0;
}

/* Init add item to list */
int 
dig_list_add ( struct ilist *list, int val ) 
{
    void *p;
    int n;
    
    if ( list->n_values == list->alloc_values ) {
	n = list->n_values + 1000;
	p = realloc ( (void *) list->value, n * sizeof(int) ); 
        if ( p == NULL ) return 0;
        list->value = (int *) p;
    }
   
    list->value[list->n_values] = val;
    list->n_values++;

    return 1;
}
