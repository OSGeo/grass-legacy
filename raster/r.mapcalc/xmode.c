/* mode function contributed by
 *  Lars Schylberg
 *  Department of Geodesy and Photogrammetry,
 *  Royal Inst. of Technology, Stockholm, Sweden
 */

#include <stdlib.h>
#include "glob.h"

static int 
icmp (const void *ii, const void *jj)
{
    const CELL *i = ii, *j = jj;
    return(*i - *j);
}

static int 
dcmp (const void *ii, const void *jj)
{
    const double *i = ii, *j = jj;
    if (*i < *j) return -1;
    if (*i > *j) return 1;
    return 0;
}

int 
i_mode (int argc, CELL *argv[], register CELL *cell, register int ncols)
{
    register int i, k, m, max_freq_i ,max_value_i;
    static int nargs = 0;
    int nv;
    static CELL *arr = NULL;
    static CELL *mfq_value = NULL;
    static CELL *mfq_count = NULL;

    if ( argc > nargs )
    {
       arr = (CELL *) G_realloc ( arr, argc*sizeof(CELL));
       mfq_count = (CELL *) G_realloc ( mfq_count, argc*sizeof(CELL)+1);
       mfq_value = (CELL *) G_realloc ( mfq_value, argc*sizeof(CELL)+1);
       nargs = argc;
    }

    while (ncols-- > 0)
    {
	nv = 0;
	for ( i=0; i<argc && nv == 0; i++)
	{
	    if (ISNULL(&argv[i][ncols]))
		nv=1;
	    else
		arr[i] = argv[i][ncols];
	}
	if(nv)
	{
	    SETNULL(&cell[ncols]);
	    continue;
	}
                                                /* sort all values */
        qsort( arr, argc, sizeof(CELL), icmp);

                                                /* sort by frequency */
        for ( m=0; m<argc; m++) 
           mfq_count[m] = 0;
        mfq_value[0] = arr[0];
        mfq_count[0] = 1;
        i = 0;

        for ( m=0; m<argc; m++)
	{
            if ( arr[m] == arr[m -1])
                mfq_count[i]++;
            else
            {
               i++;
               mfq_value[i] = arr[m];
               mfq_count[i]++;
            }
        }
                                      /* determine which frequency that  */
                                      /* occurs the most                 */
                                      /* > for lowest value when ties    */
                                      /* >= for higest value when ties   */
        max_freq_i = 0;               /* don't return a zero value       */
        max_value_i = 0;              /* unless all zeros                */
        for (k=0; k<=i; k++ )
        {                            
            if ( ((mfq_count[k]) > max_freq_i) && !(mfq_value[k] == 0) )  
            {
                max_value_i = k;             
                max_freq_i = mfq_count[k];
            }
        }       
        cell[ncols] = mfq_value[max_value_i];
    }

    return 0;
}

int 
x_mode (int argc, double *argv[], register double *xcell, register int ncols)
{
    register int i, k, m, max_freq_i ,max_value_i;
    int nv;
    static int nargs = 0;
    static double *arr = NULL;
    static double *mfq_value = NULL;
    static double *mfq_count = NULL;

    if ( argc > nargs )
    {
       arr = (double *) G_realloc ( arr, argc*sizeof(double));
       mfq_count = (double *) G_realloc ( mfq_count, argc*sizeof(double)+1);
       mfq_value = (double *) G_realloc ( mfq_value, argc*sizeof(double)+1);
       nargs = argc;
    }


    while (ncols-- > 0)
    {
	nv = 0;
	for ( i=0; i<argc && nv==0; i++)
	{
	    if (ISNULL_D(&argv[i][ncols]))
		nv = 1;
	    else
		arr[i] = argv[i][ncols];
	}
	if(nv)
	{
	    SETNULL_D(&xcell[ncols]);
	    continue;
	}
                                                /* sort all values */
        qsort( arr, argc, sizeof(CELL), icmp);
                                                /* sort all values */
        qsort( arr, argc, sizeof(double), dcmp);

                                                /* sort by frequency */
        for ( m=0; m<argc; m++) 
           mfq_count[m] = 0;
        mfq_value[0] = arr[0];
        mfq_count[0] = 1;
        i = 0;

        for ( m=0; m<argc; m++)
	{
            if ( arr[m] == arr[m -1])
                mfq_count[i]++;
            else
            {
               i++;
               mfq_value[i] = arr[m];
               mfq_count[i]++;
            }
        }
                                      /* determine which frequency that  */
                                      /* occurs the most                 */
                                      /* > for lowest value when ties    */
                                      /* >= for higest value when ties   */
        max_freq_i = 0;               /* don't return a zero value       */
        max_value_i = 0;              /* unless all zeros                */
        for (k=0; k<=i; k++ )
        {                            
            if ( ((mfq_count[k]) > max_freq_i) && !(mfq_value[k] == 0) )  
            {
                max_value_i = k;
                max_freq_i = mfq_count[k];
            }
        }       
        xcell[ncols] = mfq_value[max_value_i];
    }

    return 0;
}

int 
n_mode (int n, char *name)
{
    if (n>1) return 1;
    fprintf (stderr, "%s - ", name);
    if (n==0)
	fprintf (stderr, "no arguments ");
    else
	fprintf (stderr, "only one argument ");
    fprintf (stderr, "specified. usage: %s(x,y[,z...])\n", name);
    return 0;
}
