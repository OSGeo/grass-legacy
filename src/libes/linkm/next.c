/*
**  Written by David Gerdes  US Army Construction Engineering Research Lab
**  	April 1992
**  Copyright 1992 USA-CERL   All rights reserved.
**
*/
#include "linkm.h"

VOID_T *
link__get_next (list)
    VOID_T *list;
{
    VOID_T **tmp;

    tmp = (VOID_T **) list;

    return *tmp;
}

void
link__set_next (a, b)
    VOID_T *a;
    VOID_T *b;
{
    VOID_T **tmp;

    tmp = (VOID_T **) a;
    *tmp = b;
}
