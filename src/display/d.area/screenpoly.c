/* ========================================================================== *
 * screenpoly.c:  Routines to  create, destroy and manipulate an array of
 *                integer points as a circular linked list.  
 *
 * Author:  Eric G. Miller
 * Date: 2001-03-10
 *
 * License:
 * 
 * Copyright: Copyright (c) 2001, Eric G. Miller <egm2@jps.net>
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * ========================================================================= */ 

/* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ *
 * NOTES:
 * The ScreenPoly routine incur some overhead, so one should consider if the
 * functionality is really useful.  In general, if you can pretty accurately
 * estimate how big of a SCREENPOLY you will need, things will work better.
 * 
 * Since this implementation uses pointers to a sequential array, some 
 * operations have to duplicate or generate a whole new SCREENPOLY in order
 * to keep the pointers correct.  Some initial testing showed having each
 * point malloc'ed or free'ed as needed incurred more overhead than doing
 * fewer big malloc's or free's.  The down side is it is harder to resize
 * the data without incurring larger memory usage (at least temporarily).
 * If the list data was much larger than the SCREENPOINT, then it would
 * probably be more efficient to use as needed allocation/deallocation
 * and some operations could be simplified.
 *
 * SCREENPOLY's will never shrink in size automatically, but will grow as
 * needed.  If one does alot of deletions, it may save some memory to do a
 * ScreenPolyCopy() into a more appropriately sized SCREENPOLY.  However,
 * you must weigh this against the extra (temporary) memory usage and copying
 * overhead that will be incurred.
 * @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "screenpoly.h"
#include "vect/digit.h"

/* for debugging stuff */
/* #define MAIN */

/* ------------------------------------------------------------------------- *
 * _FindOpening(): Takes a SCREENPOLY pointer, and finds the first available
 * SCREENPOINT using the _avail flags.  This routine should never be called
 * from outside this file
 * ------------------------------------------------------------------------- */
static uint32_t
_FindOpening (SCREENPOLY *sp)
{
    uint32_t i, where, size;
    
    if (sp == NULL)
    {
        perror ("_FindOpening called with NULL SCREENPOLY struct");
        exit (EXIT_FAILURE);
    }

    if (sp->_alloced <= 0)
    {
        perror ("_FindOpening called but no memory was allocated");
        exit (EXIT_FAILURE);
    }
    
    /* _alloced is alway a multiple of 8 (a byte) */
    size = sp->_alloced / 8;
    where = sp->_alloced;
    /* loop through the flags, until one of the bits is true
     * set the return index equal to the adjusted index
     */
    for (i = 0; i < size ; i++)
    {
        register uint8_t j = sp->_avail[i];
        register uint8_t mask = (1 << 7);
        
        if (!j)  /* All zeros */
            continue;
        
        if ((j & mask))
        {
            where = i * 8;
            break;
        }
        else if ((j & (mask >> 1)))
        {
            where = (i * 8) + 1;
            break;
        }
        else if ((j & (mask >> 2)))
        {
            where = (i * 8) + 2;
            break;
        }
        else if ((j & (mask >> 3)))
        {
            where = (i * 8) + 3;
            break;
        }
        else if ((j & (mask >> 4)))
        {
            where = (i * 8) + 4;
            break;
        }
        else if ((j & (mask >> 5)))
        {
            where = (i * 8) + 5;
            break;
        }
        else if ((j & (mask >> 6)))
        {
            where = (i * 8) + 6;
            break;
        }
        else if ((j & (mask >> 7)))
        {
            where = (i * 8) + 7;
            break;
        }
    } /* for() */

    if (where == sp->_alloced)
    {
        perror ("_FindOpening: No Openings found!");
        exit (EXIT_FAILURE);
    }

    return where;
} /* _FindOpening() */

/* ------------------------------------------------------------------------- *
 * _FindUsed():  Returns the index for the current "this" in the SCREENPOLY
 * array.  This function should never be called from outside this file.
 * ------------------------------------------------------------------------- */
static uint32_t
_FindUsed (SCREENPOLY *sp)
{
    if (sp == NULL)
    {
        perror ("_FindUsed: called with NULL SCREENPOLY struct");
        exit (EXIT_FAILURE);
    }
    if (sp->count < 1)
    {
        perror ("_FindUsed: called, but no SCREENPOINT's are used");
        exit (EXIT_FAILURE);
    }

    /* "this" will always be greater or equal to "_array", at least pointer
     * difference is supposed to work that way!  Therefore, we can safely
     * cast to an unsigned integer.
     */
    return (uint32_t) (sp->this - sp->_array);
}

/* ------------------------------------------------------------------------- *
 * _SetUsed(): Sets the bit flag in the _avail array to 0 at the correspond- 
 * ing "where" to the _array.   This function should not be called from out-
 * side of this file.
 * ------------------------------------------------------------------------- */
static void 
_SetUsed (SCREENPOLY *sp, uint32_t where)
{
    uint32_t i;
    uint8_t mask = (1 << 7);
    
    if (sp == NULL)
    {
        perror ("_SetUsed called with NULL SCREENPOLY struct");
        exit (EXIT_FAILURE);
    }

    if (sp->_alloced <= where)
    {
        perror ("_SetUsed called with 'where' out of range");
        exit (EXIT_FAILURE);
    }
    
    i = where / 8;

    mask >>= (where % 8);

    /* If the flag is 1, set it to zero, otherwise error */
    if (sp->_avail[i] & mask)
        sp->_avail[i] ^= mask;
    else
    {
        perror ("_SetUsed called for bit that was already set used");
        exit (EXIT_FAILURE);
    }
}

/* ------------------------------------------------------------------------- *
 * _SetFree(): Toggle the bit flag in the _avail array to 1 at the position
 * corresponding to the _array element "where".  This function should not be
 * called from outside this file.
 * ------------------------------------------------------------------------- */
static void 
_SetFree (SCREENPOLY *sp, uint32_t where)
{
    uint32_t i;
    uint8_t mask = (1 << 7);

    if (sp == NULL)
    {
        perror ("_SetFree: called with NULL SCREENPOLY struct");
        exit (EXIT_FAILURE);
    }

    if (sp->_alloced <= where)
    {
        perror ("_SetFree: called with 'where' out of range");
        exit (EXIT_FAILURE);
    }

    i = where / 8;

    mask >>= (where % 8);

    if (sp->_avail[i] & mask)
    {
        perror ("_SetFree: called for bit that is already free");
        exit (EXIT_FAILURE);
    }
    sp->_avail[i] |= mask;
}


/* ------------------------------------------------------------------------- *
 * ScreenPolyNew(): Function to create a new SCREENPOLY instance.  Various
 * initializations are done, including initial size allocation.  Callers
 * should try to pass a size that will be close to what is needed.  The real
 * allocated size will always be rounded up to the next multiple of 8.  This
 * is so our pointer bit flag array will match up correctly.
 * ------------------------------------------------------------------------- */
SCREENPOLY *
ScreenPolyNew (uint32_t sz)
{
    SCREENPOLY *new;
    uint32_t i, size;

    /* Round up to nearest size divisible by 8 */
    size = sz;
    while ((size % 8) != 0)
        size++;

    new = (SCREENPOLY *) malloc (sizeof(SCREENPOLY));

    if (new == NULL)
    {
        perror ("new SCREENPOLY malloc failed");
        exit (EXIT_FAILURE);
    }

    new->_array = (SCREENPOINT *) malloc (size * sizeof(SCREENPOINT));

    if (new->_array == NULL)
    {
        perror ("new SCREENPOINT array malloc failed");
        exit (EXIT_FAILURE);
    }
    
    new->_alloced = size;
    
    /* Make bitfield size... */
    size /= 8;
    
    new->_avail = (uint8_t *) malloc (size);

    if (new->_avail == NULL)
    {
        perror ("new _avail array malloc failed");
        exit (EXIT_FAILURE);
    }

    new->count = 0;
    new->this = NULL;
   
    /* Initialize _avail bitfield the TRUE */
    for (i = 0; i < size; i++)
        new->_avail[i] = 0xFF;
    
    return new;
}

/* ------------------------------------------------------------------------- *
 * ScreenPolyDestroy(): Delete a SCREENPOLY and all of its memory chunks.
 * Trying to use a pointer to a SCREENPOLY after calling this function is a
 * real bad idea...
 * ------------------------------------------------------------------------- */
int
ScreenPolyDestroy (SCREENPOLY *sp)
{
    if (sp == NULL)
        return -1;

    if (sp->_alloced > 0)
    {
        free (sp->_array);
        free (sp->_avail);
    }
    free (sp);
    
    return 0;
}

/* ------------------------------------------------------------------------- *
 * ScreenPolyCopy(): Copies the "old" SCREENPOLY to the "new" SCREENPOLY.
 * Elements will be in the correct order, though all pointers will of course
 * be different.
 * ------------------------------------------------------------------------- */
int
ScreenPolyCopy (SCREENPOLY *old, SCREENPOLY *new)
{
    uint32_t i;
    SCREENPOINT *this;

    if (old == NULL || new == NULL)
    {
        perror ("ScreenPolyCopy: got NULL for \"old\" or \"new\"");
        exit (EXIT_FAILURE);
    }
    if (old->count > new->_alloced)
    {
        perror ("ScreenPolyCopy: \"new\" is smaller than \"old\"");
        exit (EXIT_FAILURE);
    }
    if (old->count < 1)
        return 0;
    if (old->count == 1)
    {
        ScreenPolyAddPoint (new, old->this->x, old->this->y);
        return 0;
    }
    /* We want the new copy to be at the same position as the old
     * when we are done.
     */
    this = old->this->next;
    for (i = 0; i < old->count; i++)
    {
        ScreenPolyAddPoint (new, this->x, this->y);
        this = this->next;
    }

    return 0;
}


/* ------------------------------------------------------------------------- *
 * ScreenPolyGrow(): Increase the size of a SCREENPOLY by at least "elems"
 * elements.  "elems" must be at least 1.  The return value is useless.
 * ------------------------------------------------------------------------- */
int
ScreenPolyGrow (SCREENPOLY *sp, uint32_t elems)
{
    SCREENPOLY  *newPoly;

    if (sp == NULL)
    {
        perror ("ScreenPolyGrow: called with NULL SCREENPOLY pointer");
        exit (EXIT_FAILURE);
    }

    if (elems == 0)
    {
        perror ("ScreenPolyGrow: called with \"elems\" equal to zero");
        exit (EXIT_FAILURE);
    }

    if (NULL == (newPoly = ScreenPolyNew (sp->_alloced + elems)))
    {
        perror ("ScreenPolyGrow: ScreenPolyNew() returned NULL");
        exit (EXIT_FAILURE);
    }

    /* Despite how dangerous this looks, the calls in ScreenPolyCopy
     * to ScreenPolyAddPoint will not cause ScreenPolyAddPoint to
     * call us again (since newPoly is now big enough).  This hackery
     * allows us to resize the internals, and make sure all the internal
     * pointers are pointing at the right thing, while still maintaining
     * the same toplevel pointer for the original SCREENPOLY.  realloc()
     * can move our stuff around, which will break all the list pointers.
     */
    ScreenPolyCopy (sp, newPoly);

    /* Get rid of the original arrays */
    free (sp->_avail);
    free (sp->_array);

    /* set the internal variables to the new stuff */
    sp->_avail   = newPoly->_avail;
    sp->_array   = newPoly->_array;
    sp->_alloced = newPoly->_alloced;
    sp->count    = newPoly->count;
    sp->this     = newPoly->this;

    /* Get rid of our toplevel SCREENPOLY pointer, leaving its
     * dynamically allocated internals intact.
     */
    free (newPoly);

    return 0;
}
    
    
/* ------------------------------------------------------------------------- *
 * ScreenPolyAddPoint():  Adds the point (x,y) and makes "this" point to it.
 * The value of "this" is linked before the new point, and the value of 
 * this->next is linked after the new point.  Unless, of course, this is the
 * first point (we can't have it pointing at itself).
 * ------------------------------------------------------------------------- */
int
ScreenPolyAddPoint (SCREENPOLY *sp, int x, int y)
{
    SCREENPOINT *before, *after, *this;
    uint32_t where;
    
    if (sp == NULL)
    {
        perror ("ScreenPolyAddPoint: Got NULL pointer");
        exit (EXIT_FAILURE);
    }

    if (sp->count == sp->_alloced)
    {
        ScreenPolyGrow (sp, 1);
    }

    where = _FindOpening (sp);

    this = &sp->_array[where];
    
    this->x = x;
    this->y = y;

    if (sp->count == 0)
    {
        /* First Entry */
        this->prev = NULL;
        this->next = NULL;
        sp->this  = this;
    }
    else if (sp->count == 1)
    {
        /* Weed out duplicates */
        /**** Causes problems
        if (this->x == sp->this->x && this->y == sp->this->y)
            return 0;
        ****/
        /* Special Case */
        sp->this->prev = this;
        sp->this->next = this;
        this->next = sp->this;
        this->prev = sp->this;
        sp->this = this;
    }
    else
    {
        /* Other Cases */
        before = sp->this;
        after  = sp->this->next;
        /* Weed out duplicates */
        /***** Causes problems
        if (     (this->x == before->x && this->y == before->y)
              || (this->x == after->x && this->y == after->y)
            )
            return 0;
        ******/
        before->next = this;
        after->prev = this;
        this->prev = before;
        this->next = after;
        sp->this = this;
    }

    sp->count++;

    _SetUsed (sp, where);

    return 0;
}

/* ------------------------------------------------------------------------- *
 * ScreenPolyDeletePoint(): "deletes" the current value of "this".  The 
 * point isn't really deleted, but bit flags and pointers are adjusted.  When
 * called, the value of "this->prev" becomes "this".  Thus, adding a point
 * pushes the current "this" to "this->prev", and deleting a point brings it
 * back.  So deletions run backwards in the circle, while additions run 
 * forward.  This fact may be important for your use.
 * ------------------------------------------------------------------------- */
int
ScreenPolyDeletePoint (SCREENPOLY *sp)
{
    uint32_t where;
    SCREENPOINT *this, *before, *after;

    if (sp == NULL)
    {
        perror ("ScreenPolyDeletePoint: called with NULL SCREENPOLY pointer");
        exit (EXIT_FAILURE);
    }
    if (sp->count < 1)
    {
        perror ("ScreenPolyDeletePoint: called, but no active points exist");
        exit (EXIT_FAILURE);
    }

    /* Flip bits */
    where = _FindUsed (sp);
    _SetFree (sp, where);

    /* Decrement count */
    sp->count--;
    
    /* Adjust pointers */
    if (sp->count == 0) /* Special Case */
    {
        sp->this = NULL;
        return 0;
    }
    if (sp->count == 1) /* Special Case */
    {
        this = sp->this->next;
        this->prev = NULL;
        this->next = NULL;
        sp->this = this;
        return 0;
    }
    /* All other cases */
    this   = sp->this;
    before = this->prev;
    after  = this->next;
    before->next = after;
    after->prev = before;
    sp->this = before;

    return 0;
}

/* ------------------------------------------------------------------------- *
 * ScreenPolyMoveNext(): Make "this->next", "this".  Useful in conjunction 
 * with ScreenPolyDeletePoint() (which deletes "backwards")
 * ------------------------------------------------------------------------- */
void
ScreenPolyMoveNext (SCREENPOLY *sp)
{
    if (sp == NULL)
    {
        perror ("ScreenPolyMoveNext: called with NULL SCREENPOLY pointer");
        exit (EXIT_FAILURE);
    }

    if (sp->count < 2)
        return;

    sp->this = sp->this->next;
}

/* ------------------------------------------------------------------------- *
 * ScreenPolyMovePrev(): Make "this->prev", "this".  Maybe useful for adding
 * points in reverse order from how you have them...
 * ------------------------------------------------------------------------- */
void
ScreenPolyMovePrev (SCREENPOLY *sp)
{
    if (sp == NULL)
    {
        perror ("ScreenPolyMovePrev: called with NULL SCREENPOLY pointer");
        exit (EXIT_FAILURE);
    }

    if (sp->count < 2)
        return;

    sp->this = sp->this->prev;
}

/* ------------------------------------------------------------------------- *
 * ScreenPolyPrint(): Utility function for printing out every point in the
 * SCREENPOLY, in order from the current "this". Format: "(x, y)\n"
 * ------------------------------------------------------------------------- */
void
ScreenPolyPrint (SCREENPOLY *sp)
{
    SCREENPOINT *this;
    
    if (sp == NULL || sp->count == 0)
        return;

    this = sp->this;
    do
    {
        fprintf (stdout, "(%d, %d)\n", this->x, this->y);
        this = this->next;
    } while (this != NULL && this != sp->this);
}


/* ------------------------------------------------------------------------- *
 * ScreenPolyMoveNearest(): Takes to SCREENPOLY's and moves their current
 * "this" entries until the distance between them is minimized.  This is
 * an expensive operation since each point may be compared.  A short circuit
 * exists to try to minimize the time.  There must be at least two points
 * in each SCREENPOLY, otherwise the function does nothing and returns -1.
 * ------------------------------------------------------------------------- */
int
ScreenPolyMoveNearest (SCREENPOLY *a, SCREENPOLY *b)
{
    uint32_t i, j;
    double dmin, dcur, dx, dy;
    SCREENPOINT *aSave, *bSave, *aThis, *bThis;

    if (a == NULL || b == NULL)
    {
        perror ("ScreenPolyMoveNearest: called with NULL SCREENPOLY pointer");
        exit (EXIT_FAILURE);
    }

    if (a->count < 2 || b->count < 2)
        return -1;

    aThis = aSave = a->this;
    bThis = bSave = b->this;
    dmin  = 1.9E20;  /* Please compiler... */
    
    for (i = 0; i < a->count; i++)
    {
        aThis = aThis->next;
        
        for (j = 0; j < b->count; j++)
        {
            bThis = bThis->next;
            
            /* Find the Square of the Distance */
            dx = (double) (bThis->x - aThis->x);
            dy = (double) (bThis->y - aThis->y);
            dcur = dx*dx + dy*dy;

            /* If first set of loops just set dmin and continue */
            if (i == 0 && j == 0)
            {
                dmin = dcur;
            }
            else if (dcur < dmin)
            {
                dmin = dcur;
                aSave = aThis;
                bSave = bThis;
            }

            /* Short circuit attempt */
            if (dmin <= 2.0)
                goto finish;
        } /* Inner for() */
    } /* Outer for() */

finish:

    a->this = aSave;
    b->this = bSave;
        
    return 0;
}  /* ScreenPolyMoveNearest */


/* ------------------------------------------------------------------------- *
 * ScreenPolyMerge(): Takes two SCREENPOLY's and returns a new one with
 * the entries of both connected where ScreenPolyMoveNearest() thinks they
 * are closest.  This method isn't really correct, but should work most of
 * the time...  You may want to call ScreenPolyDestroy() for the two 
 * SCREENPOLY's sent  to this function after it returns successfully.
 * NOTE: If either SCREENPOLY has a point count less than 2, it is silently
 * dropped (if you want to add a point use ScreenPolyAddPoint).  If both
 * have a point count less than two, a NULL pointer is returned.
 * ------------------------------------------------------------------------- */
SCREENPOLY *
ScreenPolyMerge (SCREENPOLY *a, SCREENPOLY *b)
{
    uint32_t i;
    SCREENPOINT *aPnt, *bPnt, *pnt;
    SCREENPOLY  *new;

    if (a == NULL || b == NULL)
    {
        perror ("ScreenPolyMerge: called with NULL SCREENPOLY pointer");
        exit (EXIT_FAILURE);
    }

    if (a->count < 2 && b->count < 2)
        return NULL;

    if (a->count < 2)
    {
        /* Special case drops "a", copies "b" */
        new = ScreenPolyNew (b->count);
        ScreenPolyCopy (b, new);
        return new;
    }
    else if (b->count < 2)
    {
        /* Special case drops "b", copies "a" */
        new = ScreenPolyNew (a->count);
        ScreenPolyCopy (a, new);
        return new;
    }
    else
    {
        /* Do the merge */
        ScreenPolyMoveNearest (a, b);

        if ((new = ScreenPolyNew (a->count + b->count + 3)) == NULL)
        {
            perror ("ScreenPolyMerge: ScreenPolyNew() failed");
            exit (EXIT_FAILURE);
        }

        /* Save branch points */
        aPnt = a->this;
        bPnt = b->this;
        
        /* First point is "a->this" */
        ScreenPolyAddPoint (new, a->this->x, a->this->y);

        /* Now, add in all of "b" */
        pnt = bPnt;
        for (i = 0; i < b->count; i++)
        {
            ScreenPolyAddPoint (new, pnt->x, pnt->y);
            pnt = pnt->next;
        }
        /* Now, add first "b" point to close the ring */
        ScreenPolyAddPoint (new, bPnt->x, bPnt->y);
        
        /* Now, add tie back point of first "a" point */
        ScreenPolyAddPoint (new, aPnt->x, aPnt->y);

        /* Make "pnt" the "next" point in "a" and add the rest of "a" */
        pnt = aPnt->next;
        for (i = 0; i < a->count; i++)
        {
            ScreenPolyAddPoint (new, pnt->x, pnt->y);
            pnt = pnt->next;
        }

        return new;
    }

    /* Never reached */
    return NULL;
}  /* ScreenPolyMerge() */


/* ------------------------------------------------------------------------- *
 * ScreenPolyClip():  Clip a SCREENPOLY to a box, returning a new SCREENPOLY.
 * If the routine returns NULL, then the SCREENPOLY was either entirely
 * outside the box, or contained only one point within the box.
 *
 * TODO:  This is not fully functional yet.  Idea is to first create vertices
 * for all intersections, then clip the poly.  There's some problems though
 * with needing a screen corner point interjected.
 * ------------------------------------------------------------------------- */

/* Find the intersection of two line segments, uses diglib routine */
static int
_SegmentIntersection (int ax1, int ay1, int ax2, int ay2,
        int bx1, int by1, int bx2, int by2,
        int *x, int *y)
{
    int ret;
    double X, Y;

    ret = dig_find_intersection (
            (double) ax1, (double) ay1, (double) ax2, (double) ay2,
            (double) bx1, (double) by1, (double) bx2, (double) by2,
            &X, &Y);
    if (ret == 1)
    {
        /* Found intersection */
        *x = (int) floor (X + 0.5);
        *y = (int) floor (Y + 0.5);
    }
    return ret;
}

/* Find the length of the segment, used for sorting */
static double
_SegmentLength (int x1, int y1, int x2, int y2)
{
    int dx, dy;
    double len;

    dx = x2 - x1;
    dy = y2 - y1;
    
    if (dx != 0 || dy != 0)
        return sqrt ((double)(dx*dx + dy*dy));
    else
        return 0.0;
}

/* A struct for new split vertices with the distance from the
 * original start point of the segment before new vertices
 * are added.
 */
struct dist_point
{
    int x, y;
    double d;
};


/* Compare function for qsort() on struct dist_point array */
int dist_point_compare (const void *a, const void *b)
{
    const struct dist_point *one = a;
    const struct dist_point *two = b;
    double diff = one->d - two->d;

    return ((diff >= 0.0) ? ((diff > 0.0) ? -1 : 0) : +1);
}

/* Is the point in the box, including the edges? -- Should be a macro */
static int
_PointInBox (int x, int y, int t, int b, int l, int r)
{
    return (x >= l && x <= r && y <= t && y >= b);
}

/* Finds the intersection of a horizontal line "Y" with a line described
 * by two points.  The intersection may be way out of the allowable
 * "universe" of points.
 */
static double
_YLineIntersectSegment (double Y, double ax, double ay, double bx, double by)
{
    double b, m, X;

    if (ax == bx)
        return ax;

    m = (by - ay) / (bx - ax);
    b = ay - m*ax;
    X = ((Y - b) / m);
    return X;

}
        

/* Attempt to weed out points outside the clipping poly, something broken
 * here...
 */
static int
_RealPolyClip (SCREENPOLY *sp, SCREENPOLY *clip)
{
    uint32_t i, j, n_crossings;
    double ax, ay, bx, by, X, Y, X2 ;
    SCREENPOINT *pnt;

    if (sp == NULL || clip == NULL)
    {
        perror ("_RealPolyClip: got NULL SCREENPOLY pointer");
        exit (EXIT_FAILURE);
    }

    if (sp->count < 2 || clip->count < 2)
        return -1;

    for (i = 0; i < sp->count && sp->count > 0; i++)
    {
        X = (double) sp->this->x;
        Y = (double) sp->this->y;
        n_crossings = 0;

        pnt = clip->this;
        for (j = 0; j < clip->count; j++)
        {
            ax = (double) (pnt->x);
            ay = (double) (pnt->y);
            bx = (double) (pnt->next->x);
            by = (double) (pnt->next->y);
            /* If ay == Y, adjust ay ever so slightly */
            if (ay == Y)
            {
                if (Y != 0.0)
                    ay *= 1.0000000001;
                else
                    ay = 0.0000000001;
            }
            
            /* Only care about segments to the right of the point
             * that intersect a horizontal ray at Y
             */
            if (((ax < X) && (bx < X)) ||
                ((ay > Y) && (by > Y)) ||
                ((ay < Y) && (by < Y)))
            {
                pnt = pnt->next;
                continue;
            }
            if ((X2 = _YLineIntersectSegment (Y, ax, ay, bx, by)) > X)
            {
                n_crossings++;
            }
            pnt = pnt->next;
        } /* inner for */

        if ((n_crossings % 2) == 0)
        {
            ScreenPolyDeletePoint (sp);
            i--;
        }
        ScreenPolyMoveNext (sp);
    } /* outer for */
    
    return 0;
}

/* TODO: This doesn't work correctly.  We really need to find the intersection
 * of the two, but this doesn't get it quite right.
 */
SCREENPOLY *
ScreenPolyClip (SCREENPOLY *sp, SCREENPOLY *clip)
{
    uint32_t i, j;
    int alloced, count, x, y, status;
    struct dist_point *dp, *tmp;
    SCREENPOINT *a, *b;
    SCREENPOLY *new;
    
    if (sp == NULL || clip == NULL)
    {
        perror ("ScreenPolyClip: Called with NULL SCREENPOLY pointer");
        exit (EXIT_FAILURE);
    }

    if (sp->count < 2 || clip->count < 2)
        return NULL;

    if ((new = ScreenPolyNew (sp->count)) == NULL)
    {
        perror ("ScreenPolyClip: ScreenPolyNew() returned NULL pointer");
        exit (EXIT_FAILURE);
    }
    
    /* This part of the code merely creates a screenpoly with extra
     * intersection points with the clipping screenpoly.  Later, we
     * remove the points outside the clipping poly.  This lets up
     * maintain connectivity and shape properly while removing points
     * outside the clipping polygon.  Undoubtedly there's a more
     * efficient way to achieve the same results.
     */
    alloced = 0;
    dp = NULL;
    a = sp->this;
    for (i = 0; i < sp->count; i++)
    {
        b = clip->this;
        count = 0;
        for (j = 0; j < clip->count; j++)
        {
            if ((status = _SegmentIntersection (
                        a->x, a->y, a->next->x, a->next->y,
                        b->x, b->y, b->next->x, b->next->y,
                        &x, &y)) != 0) /* no intersection */
            {
                if (status == 1) /* unique */
                    count++;
                else if (status == -1) /* colinear */
                    count += 2;
                else /* huh */
                    G_fatal_error ("Unexpected return value from "\
                            "_SegmentIntersection");
                if (count > alloced)
                {
                    tmp = (struct dist_point *) 
                           G_realloc (dp, sizeof (struct dist_point) * count);
                    if (tmp == NULL)
                    {
                        perror ("ScreenPolyClip: G_realloc failed!");
                        exit (EXIT_FAILURE);
                    }
                    dp = tmp;
                    alloced = count;
                }
                if (status ==  1)
                {
                    dp[count-1].x = x;
                    dp[count-1].y = y;
                    dp[count-1].d = _SegmentLength (a->x, a->y, x, y);
                }
                else
                {
                    /* determine which points to use for a new segment */
                    if (_PointInBox (a->x, a->y, 
                                b->x, b->y, b->next->x, b->next->y))
                    {
                        dp[count-2].x = a->x;
                        dp[count-2].y = a->y;
                        dp[count-2].d = 0.0;
                    }
                    else
                    {
                        dp[count-2].x = b->x;
                        dp[count-2].y = b->y;
                        dp[count-2].d = _SegmentLength (a->x, a->y, b->x, b->y);
                    }
                    if (_PointInBox (a->next->x, a->next->y,
                                b->x, b->y, b->next->x, b->next->y))
                    {
                        dp[count-1].x = a->next->x;
                        dp[count-1].y = a->next->y;
                        dp[count-1].d = _SegmentLength (a->x, a->y,
                                        a->next->x, a->next->y);
                    }
                    else
                    {
                        dp[count-1].x = b->next->x;
                        dp[count-1].y = b->next->y;
                        dp[count-1].d = _SegmentLength (a->x, a->y,
                                            b->next->x, b->next->y);
                    }
                }
            } /* if ((status = _SegmentIntersection(...)) != 0)) */
            b = b->next;
        } /* end inner for() */
        ScreenPolyAddPoint (new, a->x, a->y);
        if (count > 0)
        {
            qsort (dp, count, sizeof(struct dist_point), dist_point_compare);
            for (j = 0; j < count; j++)
                ScreenPolyAddPoint (new, dp[j].x, dp[j].y);
        }
        a = a->next;
    }

    G_free (dp);
    
    /* Now really weed out those points outside the clipping poly */
    /* TODO: this isn't working */
    /* _RealPolyClip (new, clip); */

    if (new->count < 2)
    {
        ScreenPolyDestroy (new);
        return NULL;
    }
    else
        return new;
} /* ScreenPolyClip() */



/* ------------------------------------------------------------------------- *
 * This takes a SCREENPOLY type and populates the "x" and "y" arrary.  The
 * return value is the number of points.  This function allocates the
 * memory, hence the double pointers.  "x" and "y" should not point to
 * anything prior to this call, and should be deallocated when they are
 * no longer needed.
 * ------------------------------------------------------------------------- */
int
ScreenPolyToArrays (SCREENPOLY *sp, int **x, int **y)
{
    int i;
    int *u, *v;
    SCREENPOINT *this;
    
    /* line needs 2 points, poly needs 3 */
    if (sp == NULL || sp->count < 2)
        return 0;

    this = sp->this;
    if (this == NULL)
    {
        G_warning ("ScreenPolyToArrays: sp->entry == NULL, but shouldn't");
        return -1;
    }
    
    /* For some reason or other, the last point may not be the same as
     * the first point.  We add an extra entry to make sure they are.
     */
    if (NULL == (u = (int *) G_malloc (sizeof (int) * (sp->count + 1))))
        return -1;
    if (NULL == (v = (int *) G_malloc (sizeof (int) * (sp->count + 1))))
    {
        G_free (u);
        return -1;
    }

    for (i = 0; i <= sp->count ; i++)
    {
        u[i] = this->x;
        v[i] = this->y;
        this = this->next;
    }

    *x = u;
    *y = v;

    return i;
}
 


#ifdef MAIN
/* ------------------------------------------------------------------------- *
 * bit_print(): Utility function to print the state of the bit flag array.
 * ------------------------------------------------------------------------- */
static void 
bit_print (uint8_t *bits, uint32_t count)
{
    uint32_t i, j;
    uint8_t mask;
    for (i = 0; i < count; i++)
    {
        mask = (1 << 7);
        for (j = 0; j < 8; j++)
            fprintf (stdout, "%d", ((mask >> j) & bits[i]) ? 0 : 1);
        fprintf (stdout, "\n");
    }
}



static void 
_print_menu (void)
{
    fprintf (stdout,
    "\n\tBitfield Menu\n"\
    "\t------------------------\n\n"\
    "\tA - Add A Point\n"\
    "\tD - Delete Current Point\n"\
    "\tF - Make Next Point the Current Point\n"\
    "\tL - Make Previous Point the Current Point\n"\
    "\tP - Print Info\n"\
    "\tQ - Quit\n\n"\
    "Bitfield: ");
    fflush(stdout);
}

int main (void)
{
    char buffer[512];
    int  size = 0, x, y;
    SCREENPOLY *pnts;

    fprintf (stdout, "Bitfield: Test bitfield accounting code\n"\
            "Bitfield: Select an initial size less than 50\n"\
            "Bitfield: ");
    fflush (stdout);
    if (fgets (buffer, 512, stdin) == NULL)
        exit(EXIT_FAILURE);
    size = atoi (buffer);
    if (size <= 0)
    {
        fprintf (stderr, "Failed to get number, bye...\n");
        exit (EXIT_FAILURE);
    }
    pnts = ScreenPolyNew (size);

    fprintf (stdout, "Number alloced = %d\n", pnts->_alloced);

    for (;;)
    {
        _print_menu();
        if (fgets (buffer, 512, stdin) == NULL)
            continue;
        switch (buffer[0])
        {
            case 'A':
            case 'a':
                fprintf (stdout, "Add Point \"x y\": "); fflush(stdout); 
                if (fgets (buffer, 512, stdin) == NULL)
                    continue;
                if ((sscanf (buffer, "%d %d", &x, &y)) != 2)
                    continue;
                if (ScreenPolyAddPoint (pnts, x, y) != 0 )
                {
                    fprintf (stderr, "Eeeek: PointsAddPoint failed!\n");
                    exit (EXIT_FAILURE);
                }
                break;
            case 'D':
            case 'd':
                if (pnts->count > 0)
                    ScreenPolyDeletePoint (pnts);
                else
                    fprintf (stderr, "No points yet!\n");
                break;
            case 'F':
            case 'f':
                if (pnts->count > 1)
                    ScreenPolyMoveNext (pnts);
                break;
            case 'L':
            case 'l':
                if (pnts->count > 1)
                    ScreenPolyMovePrev (pnts);
                break;
            case 'P':
            case 'p':
                fprintf (stdout, "\nNumber alloced = %d\n", pnts->_alloced);
                fprintf (stdout, "Bits used:\n");
                bit_print (pnts->_avail, pnts->_alloced / 8);
                ScreenPolyPrint (pnts);        
                break;
            case 'Q':
            case 'q':
                ScreenPolyDestroy (pnts);
                exit (EXIT_SUCCESS);
                break;
            default:
                fprintf (stderr, "Read the menu!\n");
                break;
        }
    }

    return 0;
}
#endif /* MAIN */

    
/* vim: set softtabstop=4 shiftwidth=4 expandtab: */
