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


#ifdef SCREENPOLY_H
/* Nothing */
#else
#define SCREENPOLY_H
/* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
 * TODO: If we could rely on stdint.h
 * we'd do "#include <stdint.h>" here.
 * Since we can't, we do the following
 * @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
 */
#if !defined(uint8_t)
  typedef unsigned char uint8_t;
#endif
#if !defined(uint32_t)
  typedef unsigned long uint32_t;
#endif


/* ------------------------------------ *
 * The SCREENPOINT and SCREENPOLY types
 * ------------------------------------ */
struct _screenpoint {
    int x,y;
    struct _screenpoint *next, *prev;
};

typedef struct _screenpoint SCREENPOINT;

struct _screenpoly {
    uint32_t _alloced, count;
    SCREENPOINT *this, *_array;
    uint8_t  *_avail;
};

typedef struct _screenpoly SCREENPOLY;

/* ------------------------------------- *
 * Function Prototypes
 * ------------------------------------- */

/* Create a new SCREENPOLY with an inital size of
 * "x" SCREENPOINT elements
 */
SCREENPOLY *ScreenPolyNew(uint32_t /* elems */);


/* Destroy a SCREENPOLY and all of its memory */
int ScreenPolyDestroy(SCREENPOLY * /* sp */);


/* Copy the data from one SCREENPOLY (first entry) to another (second).
 * The new SCREENPOLY should have already been created with ScreenPolyNew()
 */
int ScreenPolyCopy(SCREENPOLY * /* old */, SCREENPOLY * /* new */);


/* Add a point (x, y) to a screenpoly.  The SCREENPOLY will
 * grow if necessary.
 */
int ScreenPolyAddPoint(SCREENPOLY * /* sp */, int /* x */, int /* y */);


/* Make a SCREENPOLY bigger by "elems" size.  
 * Usually you don't need this, however it could be useful if you know
 * you will add a large number of points above what was originally
 * specified in ScreenPolyNew().
 */
int ScreenPolyGrow (SCREENPOLY * /* sp */, uint32_t /* elems */);


/* Delete the current point referenced by "sp->this".
 * sp->this->prev will become sp->this unless there are no more points.
 * NOTE: The actual amount of memory used will be unchanged.  If you delete
 * a very large number of points, it may be worthwhile to use ScreenPolyCopy
 * to move the data to a more modest sized SCREENPOLY.  There's overhead
 * with ScreenPolyCopy, so you'll have to guess what scenario is better.
 */
int ScreenPolyDeletePoint (SCREENPOLY * /* sp */);


/* Print each SCREENPOINT elements as "(x, y)\n" to standard output. You
 * should probably supply your own printing function.
 */
void ScreenPolyPrint(SCREENPOLY * /* sp */);


/* Make "this" equal to "this->next", essentially iterating forward */
void ScreenPolyMoveNext (SCREENPOLY * /* sp */);


/* Make "this" equal to "this->prev", essentially iterating backward */
void ScreenPolyMovePrev (SCREENPOLY * /* sp */);


/* ------------------------------------------------------------------------- *
 * ScreenPolyMoveNearest(): Takes to SCREENPOLY's and moves their current
 * "this" entries until the distance between them is minimized.  This is
 * an expensive operation since each point may be compared.  A short circuit
 * exists to try to minimize the time.  There must be at least two points
 * in each SCREENPOLY, otherwise the function does nothing and returns -1.
 * ------------------------------------------------------------------------- */
int ScreenPolyMoveNearest (SCREENPOLY * /* a */, SCREENPOLY * /* b */);


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
SCREENPOLY * ScreenPolyMerge (SCREENPOLY * /* a */, SCREENPOLY * /* b */);


/* ------------------------------------------------------------------------- *
 * ScreenPolyClip: Clips the first SCREENPOLY to the extents of the second
 * returning a new SCREENPOLY.  NULL is returned if the new SCREENPOLY has
 * less than 2 points.
 * ------------------------------------------------------------------------- */
SCREENPOLY * ScreenPolyClip (SCREENPOLY * /* sp */, SCREENPOLY * /* clip */);


/* ------------------------------------------------------------------------- *
 * This takes a SCREENPOLY type and populates the "x" and "y" arrary.  The
 * return value is the number of points.  This function allocates the
 * memory, hence the double pointers.  "x" and "y" should not point to
 * anything prior to this call, and should be deallocated when they are
 * no longer needed.
 * ------------------------------------------------------------------------- */
int ScreenPolyToArrays (SCREENPOLY * /* sp */, int ** /* x */, int ** /* y */);


#endif /* SCREENPOLY_H */
