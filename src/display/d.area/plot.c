/* plot1() - Level One vector reading */
/* --------------------------------------------------------------
 * 2000-02-25: Eric G. Miller <egm2@jps.net>
 * Added ScreenPoly code to handle islands.  We don't want outer
 * boundary filling islands that are really holes.  Basically, the
 * code connects the outer ring with the inner ring(s) via two
 * colinear line segments.
 */
/* 12-30-1999 Bill Hughes
     Changed to dynamic allocation of x_screen, y_screen to remove the
     4096 vector line limit. */

#include "gis.h"
#include "raster.h"
#include "display.h"
#include "Vect.h"
#include "local_proto.h"

extern int fillcolor;
extern int linecolor;
extern struct Cell_head window;

/* Polygon circular linked list of screen points */
typedef struct _ScreenPoint {
    int x, y;
    struct _ScreenPoint *next, *prev;
} SCREENPOINT;

typedef struct {
    int count;
    SCREENPOINT *entry;
} SCREENPOLY;


/* Allocate memory for new SCREENPOLY type and intialize */
static SCREENPOLY *
ScreenPolyNew (void)
{
    SCREENPOLY *new = (SCREENPOLY *) G_calloc (sizeof (SCREENPOLY), 1);
    if (new == NULL)
        return NULL;

    new->count = 0;
    new->entry = NULL;

    return new;
}

/* Push entries into the SCREENPOLY. The new entry is placed just
 * after the current entry, and then made the current entry.  If
 * an entry is passed that would duplicate the current entry, it
 * is silently dropped.  Callers should rely on sp->count, for
 * an accurate number of entries.
 */
static int
ScreenPolyAddEntry (SCREENPOLY *sp, int x, int y)
{
    SCREENPOINT *pnt, *before, *after;

    if (sp == NULL)
        return -1;
    
    if (NULL == (pnt = (SCREENPOINT *) 
                G_calloc (sizeof (SCREENPOINT), 1)))
        return -1;

    pnt->x = x;
    pnt->y = y;
    
    if (sp->count == 0)
    {
        /* First entry */
        sp->entry = pnt;
        sp->entry->next = NULL;
        sp->entry->prev = NULL;
        sp->count++;
    }
    else if (sp->count == 1)
    {
        pnt->prev = sp->entry;
        pnt->next = sp->entry;
        sp->entry->next = pnt;
        sp->entry->prev = pnt;
        sp->entry = sp->entry->next;
        sp->count++;
    }
    else
    {
        /* Weed out duplicate entries with no purpose */
        if (pnt->x == sp->entry->x && pnt->y == sp->entry->y)
        {
            G_free (pnt);
            return 0;
        }
        after = sp->entry->next;
        before = sp->entry;
        after->prev = pnt;
        before->next = pnt;
        pnt->prev = before;
        pnt->next = after;
        sp->entry = pnt;
        sp->count++;
    }

    return 0;
}

/* Removes the current entry, make the "next" entry, the current
 * entry point, and adjust "next", "prev" pointers
 */
static int
ScreenPolyRemoveEntry (SCREENPOLY *sp)
{
    SCREENPOINT *before, *after;

    if (sp == NULL || sp->count < 1)
        return -1;

    if (sp->count == 1)
    {
        /* special case */
        sp->count--;
        G_free (sp->entry);
        sp->entry = NULL;
    }
    else
    {
        before = sp->entry->prev;
        after  = sp->entry->next;
        before->next = after;
        after->prev = before;
        sp->count--;
        G_free (sp->entry);
        sp->entry = after;
    }

    return 0;
}

/* Deallocate all of the entries and free the memory for the
 * SCREENPOLY type
 */
static int
ScreenPolyDestroy (SCREENPOLY *sp)
{
    if (sp == NULL)
        return -1;

    while (ScreenPolyRemoveEntry (sp) == 0)
        ;

    G_free (sp);

    return 0;
}

/* Dumps the SCREENPOLY list -- for debugging */
#ifdef DEBUG
static void
ScreenPolyDump (SCREENPOLY *sp)
{
    int i;
    SCREENPOINT *p;
    p = sp->entry;
    for (i = 0; i < sp->count ; i++)
    {
        fprintf (stderr, "%d = {%d, %d}\n", i, p->x, p->y);
        p = p->next;
    }
}
#endif

/* Copy one SCREENPOLY to a new SCREENPOLY */
static SCREENPOLY *
ScreenPolyCopy (SCREENPOLY *sp)
{
    int i;
    SCREENPOLY *cp;
    SCREENPOINT *pnt;

    if (sp == NULL)
        return NULL;
    if (NULL == (cp = ScreenPolyNew()))
        return NULL;
    
    pnt = sp->entry;
    for (i = 0; i < sp->count; i++)
    {
        ScreenPolyAddEntry (cp, pnt->x, pnt->y);
        pnt = pnt->next;
    }

    return cp;
}

/*  The idea here is to move the entry points for "a" and "b", so that
 *  the distance between their respective points is minimized.  This
 *  thing is a real CPU hog and is where the most band for the
 *  optimization buck could be had.
 */
static int
ScreenPolyMoveNearest (SCREENPOLY *a, SCREENPOLY *b)
{
    int i, j;
    double dmin, dcur, dx, dy;
    SCREENPOLY  *u, *v;
    SCREENPOINT *uPnt, *vPnt, *uSave, *vSave;

    if (a == NULL || b == NULL || a->count < 1 || b->count < 1)
        return -1;

    if (a->count > b->count)
    {
        u = b; v = a;
    }
    else
    {
        u = a; v = a;
    }

    for (i = 0 ; i < u->count ; i++)
    {
        if (i == 0)
            uPnt = uSave = u->entry;
        else
            uPnt = uPnt->next;

        for (j = 0; j < v->count; j++)
        {
            if (j == 0)
            {
                if (i == 0)
                    vSave = v->entry;
                vPnt = v->entry;
            }
            else
            {
                vPnt = vPnt->next;
            }

            /* Find the square of the distance */
            dx = (double)(uPnt->x - vPnt->x);
            dy = (double)(uPnt->y - vPnt->y);
            dcur = dx*dx + dy*dy;
            if (i == 0 && j == 0)
            {
                dmin = dcur;
            }
            else if (dcur < dmin)
            {
                uSave = uPnt;
                vSave = vPnt;
                dmin = dcur;
            }

            if (dmin < 1.0)
                goto finish;
        } /* inner for */
    } /* outer for */

finish:
    
    u->entry = uSave;
    v->entry = vSave;

    return 0;
} /* ScreenPolyMoveNearest() */


/* This takes two SCREENPOLY types, creates a new one, and merges "b" into "a"
 * connecting them at the current "entry" points.  The "a" SCREENPOLY should
 * be the outer ring.  NOTE, the origninals are copied to a new struct, so
 * you probably want to deallocate those old structs unless you need them
 * for something.
 */
static SCREENPOLY *
ScreenPolyMerge (SCREENPOLY *a, SCREENPOLY *b)
{
    int i;
    SCREENPOINT *apnt, *bpnt, *pnt;
    SCREENPOLY *new;

    if (a == NULL || b == NULL)
        return NULL;

    
    if (a->count < 1)
    {
        /* Special case, just copy "a" */
        return ScreenPolyCopy (a);
    }
    else if (b->count < 1)
    {
        /* Special case, just copy "b" */
        return ScreenPolyCopy (b);
    }
    else
    {
        /* Make sure polys are at nearest node points */
        ScreenPolyMoveNearest (a,b);

        if (NULL == (new = ScreenPolyNew()))
            return NULL;

        /* Save branch points */
        apnt = a->entry;
        bpnt = b->entry;
        
        /* First point is a->entry */
        ScreenPolyAddEntry (new, apnt->x, apnt->y);
        /* Now add all of "b" */
        pnt = bpnt;
        for (i = 0 ; i < b->count; i++)
        {
            ScreenPolyAddEntry (new, pnt->x, pnt->y);
            pnt = pnt->next;
        }
        /* Now add bpnt to make sure ring is "closed" */
        ScreenPolyAddEntry (new, bpnt->x, bpnt->y);
        /* Now add all of "a" to close the polygon */
        pnt = apnt;
        for (i = 0; i < a->count; i++)
        {
            ScreenPolyAddEntry (new, pnt->x, pnt->y);
            pnt = pnt->next;
        }
    } /* else */

    return new;
    
} /* ScreenPolyMerge */


/* This takes a SCREENPOLY type and populates the "x" and "y" arrary.  The
 * return value is the number of points.  This function allocates the
 * memory, hence the double pointers.  "x" and "y" should not point to
 * anything prior to this call, and should be deallocated when they are
 * no longer needed.
 */
static int
ScreenPolyToArrays (SCREENPOLY *sp, int **x, int **y)
{
    int i, *u, *v;
    SCREENPOINT *this;
    
    if (sp == NULL || sp->count < 1)
        return 0;

    this = sp->entry;
    if (this == NULL)
    {
        G_warning ("ScreenPolyToArrays: sp->entry == NULL, but shouldn't");
        return -1;
    }
    
    if (NULL == (u = (int *) G_malloc (sizeof (int) * sp->count)))
        return -1;
    if (NULL == (v = (int *) G_malloc (sizeof (int) * sp->count)))
    {
        G_free (u);
        return -1;
    }

    /* special case */
    if (sp->count == 1)
    {
        **x = this->x;
        **y = this->y;
        return 1;
    }
    
    for (i = 0; i < sp->count ; i++)
    {
        u[i] = this->x;
        v[i] = this->y;
        this = this->next;
    }

    *x = u;
    *y = v;
    
    return i;
}
    
int plot1 (char *name, char *mapset, struct line_pnts *Points)
{
    int i, j, dofill;
    struct Map_info Map;
    double N, S, E, W;
    int line, nisles;
    int nlines;
    int *x_screen, *y_screen;
    struct line_pnts *Isle;
    P_AREA *pa;
    SCREENPOLY *spArea, *spLine, *spIsle, *spTmp;

    i = Vect_open_old (&Map, name, mapset);

    if (2 > i)
	G_fatal_error ("Failed opening vector file");

    G_setup_plot (
	D_get_d_north(), D_get_d_south(), D_get_d_west(), D_get_d_east(),
	D_move_abs, D_cont_abs);


    fprintf (stderr,"Plotting ... "); fflush (stderr);
    nlines = V2_num_areas(&Map);
    for (line = 1;line <= nlines; line++)
    {
        dofill = V2_area_att (&Map, line); /* Returns 0 if area is unlabelled */
        /* Skip areas that we wont fill or draw outlines for */
        if (!dofill && linecolor <= 0)
            continue;
        
	switch (Vect_get_area_points(&Map, line, Points))
	{
	case -1:
	    Vect_close (&Map);
	    fprintf (stderr, "\nERROR: vector file [%s] - can't read\n", name);
	    return -1;
	    break;
	case -2:
	    fprintf (stdout,"Done\n");
	    Vect_close (&Map);
	    return  0;
	    break;
	}

	V2_get_area_bbox(&Map, line, &N, &S, &E, &W);
	if ( S > window.north || N < window.south || 
	     W > window.east || E < window.west)
		continue;

        /* Get a SCREENPOLY type for the polygon line */
        spLine = ScreenPolyNew();
	for(i=0; i < Points->n_points; i++)
	{
            /* Populate the SCREENPOLY type with screen coordinates */
            ScreenPolyAddEntry (
                spLine,
                (int) (D_u_to_d_col (Points->x[i])),
                (int) (D_u_to_d_row (Points->y[i]))
            );
	}

        /* Are we filling this poly */
        if (dofill)
        {
            /* Make copy of polygon */
            spArea = ScreenPolyCopy (spLine);
       
            /* Need to do extra work to handle islands */
            V2_get_area (&Map, line, &pa);
            if ((nisles = pa->n_isles) > 0)
            {
                /* Then we need to "merge" in island polys */
                for (i = 0; i < nisles ; i++)
                {
                    Isle = Vect_new_line_struct();
                    Vect_get_isle_points (&Map, pa->isles[i], Isle);
                    spIsle = ScreenPolyNew();
                    for (j = 0; j < Isle->n_points; j++)
                        ScreenPolyAddEntry (
                                spIsle, 
                                (int) (D_u_to_d_col (Isle->x[j])),
                                (int) (D_u_to_d_row (Isle->y[j]))
                        );
                    Vect_destroy_line_struct (Isle);
                    spTmp = ScreenPolyMerge (spArea, spIsle);
                    ScreenPolyDestroy (spIsle);
                    ScreenPolyDestroy (spArea);
                    if (spTmp == NULL)
                        G_fatal_error ("Failed to merge islands with outer poly");
                    spArea = spTmp;
                }
                    
            }
            
            i = ScreenPolyToArrays (spArea, &x_screen, &y_screen);
            ScreenPolyDestroy (spArea);

            if (i == 0)
            {
                G_warning ("No points in point struct ??");
                G_free (x_screen);
                G_free (y_screen);
                continue;
            }
            else if (i < 0)
                G_fatal_error ("Converting ScreenPoly to Arrays");
            else
            {
                R_standard_color(fillcolor);
                R_polygon_abs(x_screen, y_screen, i);
            }
            G_free (x_screen);
            G_free (y_screen);
            
        }

        /* Are we drawing poly outlines ? */
	if (linecolor > 0)
	{
            i = ScreenPolyToArrays (spLine, &x_screen, &y_screen);   
	    R_standard_color(linecolor);
	    R_polyline_abs(x_screen,y_screen, i);
            G_free(x_screen);
            G_free(y_screen);
	}
        ScreenPolyDestroy (spLine);

        /* Percentages */
        G_percent (line, nlines, 5);
    }
    /* do newline */
    fprintf (stderr, "\n");
    return 0;
}


/* vim: set softtabstop=4 shiftwidth=4 expandtab: */
