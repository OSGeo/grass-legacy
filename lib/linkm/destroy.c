/*
**  Written by David Gerdes  US Army Construction Engineering Research Lab
**  	April 1992
**  Copyright 1992 USA-CERL   All rights reserved.
**
*/
#include "linkm.h"


void 
link_destroy (struct link_head *Head, VOID_T *ptr)
{
    if (NULL == ptr)
	return;

    link__set_next (ptr, Head->Unused);		/* ptr->next = Unused */
    Head->Unused = ptr;				/* Unused = ptr */
}
