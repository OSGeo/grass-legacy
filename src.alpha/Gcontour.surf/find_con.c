#include "contour.h"

find_con_slow (r,c,d1,d2,con1,con2)
SHORT r,c;
double *d1, *d2;
CELL *con1, *con2;
{
	NODE	*addpts_slow ();
	int	ct, low_ct, node_ct;
	SHORT	rr, cc, dor, doc;
	double	dd, sqrt(), shortest;
	CELL	value;

	*con1 = 0;
	*con2 = 0;
	*d1 = *d2 = 1.0;
	shortest = nrows * ncols;
	for (rr = minr; rr <=maxr; rr++)
	{
		for (cc = minc; cc <=maxc; cc++)
		{
			/* FLAG_UNSET (seen, rr, cc); */
			bseg_put (&bseen, &off, rr, cc);
		}
	}
	minr = nrows;
	minc = ncols;
	maxr = maxc = -1;
	/* set_seen_slow (r,c); */
	bseg_put (&bseen, &on, r, c);
	if (r < minr) minr = r;
	if (r > maxr) maxr = r;
	if (c < minc) minc = c;
	if (c > maxc) maxc = c;
	node_ct = 0;
	zero = addpts_slow (zero,r,c,r,c,&node_ct);
	low_ct = 0;
	while(1)	{
		ct = low_ct++;
		if(node_ct <= ct)
			return;
		rr = zero[ct].r;
		cc = zero[ct].c;
		dor = ABS(rr-r);
		doc = ABS(cc-c);
		if(rr >= 0 && cc >= 0 && rr < nrows && cc < ncols &&
		    zero[ct].d < shortest)	
		{
			cseg_get (&con, rr, cc, &value);
		    	if (value == 0)	
			{
				zero = addpts_slow (zero,r,c,rr,cc,&node_ct);
		    	} 
			else if(*con1 == 0)	
			{
				*con1 = value;
				*d1 = MIN(dor,doc) * 1.414 + ABS(dor - doc);
				shortest = *d1 * 2.0 * i_val_l_f;
			} 
			else if(*con1 == value)	
			{
				dd = MIN(dor,doc) * 1.414 + ABS(dor - doc);
				if(dd < *d1) 	
				{
					*d1 = dd;
					shortest = dd * 2.0 * i_val_l_f;
				}
			} 
			else if(*con2 == 0)	
			{
				*con2 = value;
				*d2 = MIN(dor,doc) * 1.414 + ABS(dor - doc);
				shortest = *d2;
			} 
			else	
			{
				dd = MIN(dor,doc) * 1.414 + ABS(dor - doc);
				shortest = MIN(shortest,dd);
			}
		}
	}
}

find_con(r,c,d1,d2,con1,con2)
SHORT r,c;
double *d1, *d2;
CELL *con1, *con2;
{
	NODE	*addpts();
	int	ct, low_ct, node_ct;
	SHORT	rr, cc, dor, doc;
	double	dd, sqrt(), shortest;
	CELL	value;

	*con1 = 0;
	*con2 = 0;
	*d1 = *d2 = 1.0;
	shortest = nrows * ncols;
	for (rr = minr; rr <=maxr; rr++)
	{
		for (cc = minc; cc <=maxc; cc++)
		{
			FLAG_UNSET (seen, rr, cc);
		}
	}
	minr = nrows;
	minc = ncols;
	maxr = maxc = -1;
	/* set_seen (r,c); */
	FLAG_SET (seen, r, c);
	if (r < minr) minr = r;
	if (r > maxr) maxr = r;
	if (c < minc) minc = c;
	if (c > maxc) maxc = c;
	node_ct = 0;
	zero = addpts(zero,r,c,r,c,&node_ct);
	low_ct = 0;
	while(1)	{
		ct = low_ct++;
		if(node_ct <= ct)
			return;
		rr = zero[ct].r;
		cc = zero[ct].c;
		dor = ABS(rr-r);
		doc = ABS(cc-c);
		if(rr >= 0 && cc >= 0 && rr < nrows && cc < ncols &&
		    zero[ct].d < shortest)	
		{
			cseg_get (&con, rr, cc, &value);
		    	if (value == 0)	
			{
				zero = addpts(zero,r,c,rr,cc,&node_ct);
		    	} 
			else if(*con1 == 0)	
			{
				*con1 = value;
				*d1 = MIN(dor,doc) * 1.414 + ABS(dor - doc);
				shortest = *d1 * 2.0 * i_val_l_f;
			} 
			else if(*con1 == value)	
			{
				dd = MIN(dor,doc) * 1.414 + ABS(dor - doc);
				if(dd < *d1) 	
				{
					*d1 = dd;
					shortest = dd * 2.0 * i_val_l_f;
				}
			} 
			else if(*con2 == 0)	
			{
				*con2 = value;
				*d2 = MIN(dor,doc) * 1.414 + ABS(dor - doc);
				shortest = *d2;
			} 
			else	
			{
				dd = MIN(dor,doc) * 1.414 + ABS(dor - doc);
				shortest = MIN(shortest,dd);
			}
		}
	}
}
