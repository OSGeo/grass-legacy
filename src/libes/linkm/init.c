/*
**  Written by David Gerdes  US Army Construction Engineering Research Lab
**  	April 1992
**  Copyright 1992 USA-CERL   All rights reserved.
**
*/
#include "linkm.h"

static int link_chunk_size = 100;
static int link_exit_flag = 0;

void
link_set_chunk_size (size)
    int size;
{
    link_chunk_size = size;
}

void 
link_exit_on_error (flag)
    int flag;
{
    link_exit_flag = flag;
}

VOID_T *
link_init (size)
    int size;
{
    
    struct link_head *Head;

    if (NULL == (Head = (struct link_head *) malloc (sizeof (struct link_head))))
	return NULL;

    if (NULL == (Head->ptr_array = (VOID_T **) malloc (sizeof (VOID_T *) * PTR_CNT)))
    {
	free (Head);
	return NULL;
    }

    Head->max_ptr = 0;
    Head->Unused = NULL;
    Head->alloced = PTR_CNT;
    Head->unit_size = size < sizeof (VOID_T *) ? sizeof (VOID_T *) : size;
    Head->chunk_size = link_chunk_size;
    Head->exit_flag = link_exit_flag;

    return (VOID_T *) Head;
}

void
link_cleanup (Head)
    struct link_head *Head;
{
    register int i;

    if (Head == NULL)
	return;
    
    if (Head->ptr_array)
    {
	for (i = 0 ; i < Head->max_ptr ; i++)
	    if (Head->ptr_array[i] != NULL)
		free (Head->ptr_array[i]);
	free (Head->ptr_array);
	free (Head);
    }
}
