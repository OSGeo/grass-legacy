/**** bbox.c ****/
/*
**  Written by David Gerdes  Fall 1992
**  US Army Construction Engineering Research Lab
**  Copyright  David Gerdes  USA-CERL  1992
*/


#include "Vect.h"
#include "cutter.h"

/*
** if the bounding boxes of the two lines intersect, return 1
** else return 0
*/
line_bboxes_cross (Aline, Bline)
    P_LINE *Aline, *Bline;
{
    if (Aline->E < Bline->W)
        return 0;
    if (Aline->W > Bline->E)
        return 0;
    if (Aline->N < Bline->S)
        return 0;
    if (Aline->S > Bline->N)
        return 0;

    return 1;
}

area_bboxes_cross (Aarea, Barea)
    P_AREA *Aarea, *Barea;
{
    if (Aarea->E < Barea->W)
        return 0;
    if (Aarea->W > Barea->E)
        return 0;
    if (Aarea->N < Barea->S)
        return 0;
    if (Aarea->S > Barea->N)
        return 0;

    return 1;
}

