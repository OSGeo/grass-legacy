#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "gis.h"
#include "gprojects.h"

/********************* PARMS FOR PROJ **********************/ 

/* a couple defines to simplify reading the function */
#define MULTIPLY_LOOP(x,y,c,m) \
do {\
   int i; \
   for (i = 0; i < c; ++i) {\
       x[i] *= m; \
       y[i] *= m; \
   }\
} while (0)
  
#define DIVIDE_LOOP(x,y,c,m) \
do {\
   int i; \
   for (i = 0; i < c; ++i) {\
       x[i] /= m; \
       y[i] /= m; \
   }\
} while (0)

static double METERS_in = 1.0, METERS_out = 1.0;

/* Improved version of pj_do_proj uses the pj_transform function instead
 * of pj_fwd and pj_inv, to take advantage of datum conversions          */


/*!
 * \brief Project x,y
 *
 * Project the given coordinate pair (x, y)
 * from a projection defined in info_in to projection defined in info_out.
 *
 *  \param x
 *  \param y
 *  \param info_in
 *  \param info_out
 *  \return int
 */

int pj_do_proj(x,y,info_in,info_out)
  double *x,*y;
  struct pj_info *info_in,*info_out;
{
    int ok;
    double u,v;
    double h = 0.0;
   
    METERS_in = info_in->meters;
    METERS_out = info_out->meters;

    if (strncmp(info_in->proj,"ll",2) == 0 ) {
        if (strncmp(info_out->proj,"ll",2) == 0 ) {
            u = (*x) / RAD_TO_DEG;
            v = (*y) / RAD_TO_DEG;
            ok = pj_transform (info_in->pj, info_out->pj, 1, 0, &u, &v, &h);
            *x = u * RAD_TO_DEG;
            *y = v * RAD_TO_DEG;
        }
        else  {
            u = (*x) / RAD_TO_DEG;
            v = (*y) / RAD_TO_DEG;
            ok = pj_transform(info_in->pj,info_out->pj, 1, 0, &u, &v, &h);
            *x = u / METERS_out;
            *y = v / METERS_out;
        }
    }
    else {
        if (strncmp(info_out->proj,"ll",2) == 0 ) {
            u = *x * METERS_in;
            v = *y * METERS_in;
            ok = pj_transform(info_in->pj,info_out->pj, 1, 0, &u, &v, &h);
            *x = u * RAD_TO_DEG;
            *y = v * RAD_TO_DEG;
        }
        else {
            u = *x * METERS_in;
            v = *y * METERS_in;
            ok = pj_transform(info_in->pj,info_out->pj, 1, 0, &u, &v, &h);
            *x = u / METERS_out;
            *y = v / METERS_out;
        }
    }
    if (ok < 0)
    {
        fprintf(stderr, "pj_transform() failed\ncause: ");
        fprintf(stderr,"%s\n",pj_strerrno(ok));
    }
    return ok;
}

/* pj_do_transform()
 * New function by Eric Miller to take advantage of height transformations and
 * transformations of multiple co-ordinates stored in arrays (as afforded by
 * PROJ.4 function pj_transform().
 * Not used yet (1/2003). */


/*!
 * \brief Project x,y,h
 *
 * Project the given coordinate triple (x, y, h)
 * from a projection defined in info_in to projection defined in info_out.
 * x, y and h should be three arrays of equal length; count is the number of
 * points to be transformed.
 *
 *  \param count
 *  \param x
 *  \param y
 *  \param h
 *  \param info_in
 *  \param info_out
 *  \return int
 */

int pj_do_transform (int count, double *x, double *y, double *h, 
		    struct pj_info *info_in, struct pj_info *info_out)
{
    int ok;
    int has_h = 1;
      
    METERS_in = info_in->meters;
    METERS_out = info_out->meters;
   
    if (h == NULL) { 
        int i;
        h = G_malloc (sizeof *h * count);
        /* they say memset is only guaranteed for chars ;-( */
        for (i = 0; i < count; ++i)
            h[i] = 0.0;
        has_h = 0;
    }
    if (strncmp(info_in->proj,"ll",2) == 0 ) {
        if (strncmp(info_out->proj,"ll",2) == 0 ) {
            DIVIDE_LOOP(x,y,count,RAD_TO_DEG);
            ok = pj_transform (info_in->pj, info_out->pj, count, 1, x, y, h);
            MULTIPLY_LOOP(x,y,count,RAD_TO_DEG);
        }
        else  {
            DIVIDE_LOOP(x,y,count,RAD_TO_DEG);
            ok = pj_transform(info_in->pj,info_out->pj, count, 1, x, y, h);
            DIVIDE_LOOP(x,y,count,METERS_out);
        }
    } 
    else {
        if (strncmp(info_out->proj,"ll",2) == 0 ) {
            MULTIPLY_LOOP(x,y,count,METERS_in);
            ok = pj_transform(info_in->pj,info_out->pj, count, 1, x, y, h);
            MULTIPLY_LOOP(x,y,count,RAD_TO_DEG);
        }
        else {
            MULTIPLY_LOOP(x,y,count,METERS_in);
            ok = pj_transform(info_in->pj,info_out->pj, count, 1, x, y, h);
            DIVIDE_LOOP(x,y,count,METERS_out);
        }
    }
    if (!has_h)  G_free (h);
   
    if (ok < 0)
    {
        fprintf(stderr, "pj_transform() failed\ncause: ");
        fprintf(stderr,"%s\n",pj_strerrno(ok));
    }
    return ok;
}
