/* this program makes an elevation cell file from a contour map
** that has been converted to cell form.
** last modified 04/01/89
** by  Chuck Ehlschlaeger
*/

#include <math.h>
#include "gis.h"
#include "flag.h"

#define NODE	struct node
#define SHORT	int
#define INIT_AR	64
#define AR_INCR	64
#define abs(x)		(((x) < 0) ? -(x) : (x))
#define min(x, y)	(((x) < (y)) ? (x) : (y))

SHORT	nrows, ncols;
SHORT	minc, minr, maxc, maxr;
int	array_size;
double	i_val_l_f;
CELL	*con;
FLAG	*seen;

NODE	{
	SHORT	r,c;
	int	d;
};

NODE	*zero;

main(argc,argv)
int argc;
char *argv[];
{
	SHORT	r,c;
	CELL	con1, con2;
	double	d1, d2;
	CELL	*alt;
	char	con_name[40], alt_name[40], *con_mapset, con_f, alt_f, i_f;
	CELL	*readmap();
	int	file_fd;
	int	cells;
	struct Cell_head	window;
	char	buf[80];

	G_gisinit(argv[0]);
	con_f = alt_f = i_f = 0;
	for (r = 1; r < argc; r++)	{
		if (sscanf (argv[r], "c=%[^\n]", con_name) == 1) con_f++;
		else if (sscanf (argv[r], "e=%[^\n]",alt_name) == 1)
				alt_f++;
		/* else if (sscanf (argv[r], "i=%lf", &i_val_l_f) == 1) i_f++;	*/
		else 	{
			sprintf(buf,"Usage: %s c=raster_with_contours e=output_elevation",
				argv[0]) ;
			G_fatal_error(buf);
		}
	}
	if((!con_f) || (!alt_f))	{
		sprintf(buf,"Usage: %s c=raster_with_contours e=output_elevation",
			argv[0]) ;
		G_fatal_error(buf);
	}
	con_mapset = G_find_file2("cell", con_name, "");
	if (!con_mapset)	{
		sprintf(buf, "contour cell file [%s] not found\n",
			con_name);
		G_fatal_error (buf);
		exit(1);
	}
	G_get_set_window (&window);
	nrows = G_window_rows();
	ncols = G_window_cols();
	if (i_f)	i_val_l_f -= 0.5;
	else		i_val_l_f = nrows + ncols;
	fprintf(stderr,"r:%d c:%d cells%d\n",(int)nrows,(int)ncols,
			(int)nrows*ncols);
	con = readmap(con_name, con_mapset);
	seen = flag_create(nrows,ncols);
	alt = (CELL *)G_malloc(nrows * ncols * sizeof(CELL));
	zero = (NODE *)G_malloc(INIT_AR * sizeof(NODE));
	minc = minr = 0;
	maxc = ncols - 1;
	maxr = nrows - 1;
	array_size = INIT_AR;
	cells = 0;
	for (r=0; r<nrows; r++)	{
		fprintf(stderr,"r:%d\n",(int)r);
		for (c=0; c<ncols; c++, cells++)	{
			/* fprintf(stderr,"c:%d\n",(int)c);	*/
			if(con[cells] != 0) alt[cells] = con[cells];
			else	{
			    find_con(r,c,&d1,&d2,&con1,&con2);
			    /* fprintf(stderr,"\nd:%lf d:%lf c:%d c:%d  ",d1,d2,con1,con2); */
			    if(con2 > 0)
				alt[cells] = (int)(d2 * con1 / (d1 + d2) +
					d1 * con2 / (d1 + d2) + 0.5);
			    else alt[cells] = con1;
			}
			/* fprintf(stderr,"%d\n",alt[cells]);	*/
		}
	}
	file_fd = G_open_cell_new(alt_name);
	if(!file_fd)
		exit(2);
	for(r=0; r<nrows; r++)
		G_put_map_row(file_fd, alt + r*ncols, r);
	fprintf(stderr,"4\n");
	G_close_cell(file_fd);
	fprintf(stderr,"5\n");
	exit(0);
}

CELL *
readmap(name, mapset)
char *name, *mapset;
{
	int fd;
	CELL *map;
	SHORT row;
	int (*get_row)();
	int G_get_map_row_nomask();
	char buf[80];

	map = (CELL *)G_malloc(nrows*ncols*sizeof(CELL));
	if((fd = G_open_cell_old(name,mapset)) < 0)	{
		sprintf(buf,"can't open map\n");
		G_fatal_error(buf);
	}
	fflush (stderr);
	get_row = G_get_map_row_nomask;
	for(row=0; row<nrows; row++)	{
		if((*get_row)(fd, map+row*ncols, row) < 0)	{
			sprintf(buf, "failed\n");
			G_fatal_error(buf);
		}
	}
	G_close_cell (fd);
	return map;
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

	*con1 = 0;
	*con2 = 0;
	*d1 = *d2 = 1.0;
	shortest = nrows * ncols;
	flag_clear_all(seen);
	set_seen(seen,r,c);
	node_ct = 0;
	zero = addpts(zero,r,c,r,c,&node_ct);
	low_ct = 0;
	while(1)	{
		ct = low_ct++;
		if(node_ct <= ct)
			return;
		rr = zero[ct].r;
		cc = zero[ct].c;
		dor = abs(rr-r);
		doc = abs(cc-c);
		if(rr >= 0 && cc >= 0 && rr < nrows && cc < ncols &&
		/*   dor <= shortest && doc <= shortest)	{ */
		    zero[ct].d < shortest)	{
		    /* fprintf(stderr,"rr:%d cc:%d ",rr,cc);	*/
		    	if(con[rr*ncols+cc] == 0)	{
				/* fprintf(stderr,"c=0 ");	*/
				zero = addpts(zero,r,c,rr,cc,&node_ct);
		    	} else if(*con1 == 0)	{
				/* fprintf(stderr,"*con1=0->%d ",con[rr*ncols+cc]);	*/
				*con1 = con[rr*ncols+cc];
		/* *d1 = sqrt((double)(dor * dor + doc * doc));	
				shortest = *d1 * 2.0 * i_val_l_f;	*/
				*d1 = min(dor,doc) * 1.414 + abs(dor - doc);
				shortest = *d1 * 2.0 * i_val_l_f;
			} else if(*con1 == con[rr*ncols+cc])	{
				/* fprintf(stderr,"*con1=%d ",con[rr*ncols+cc]);	*/
		/* dd = sqrt((double)(dor * dor + doc * doc));	*/
				dd = min(dor,doc) * 1.414 + abs(dor - doc);
				if(dd < *d1) 	{
					*d1 = dd;
					shortest = dd * 2.0 * i_val_l_f;
				}
			} else if(*con2 == 0)	{
				/* fprintf(stderr,"*con2=0->%d ",con[rr*ncols+cc]);	*/
				*con2 = con[rr*ncols+cc];
		/* *d2 = sqrt((double)(dor * dor + doc * doc));		*/
				*d2 = min(dor,doc) * 1.414 + abs(dor - doc);
				shortest = *d2;
/*				if(dor > doc)	{
					if(dor < shortest) shortest = dor;
				} else	if(doc < shortest) shortest = doc;	*/
			} else	{
				/* fprintf(stderr,"*con2=x ");	*/
		/* dd = sqrt((double)(dor * dor + doc * doc));		*/
				dd = min(dor,doc) * 1.414 + abs(dor - doc);
				shortest = min(shortest,dd);
				/*
				if(dd < *d2)	{
					*d2 = dd;
					*con2 = con[rr*ncols+cc];	
					if(dor > doc)	{
						if(dor < shortest)
							shortest = dor;
					} else	if(doc < shortest)
							shortest = doc;
				}	*/
			}
		}
	}
}

NODE *
addpts(zero,r,c,rr,cc,node_ct)
NODE *zero;
SHORT r, c, rr, cc;
int *node_ct;
{
	NODE *add_in();
	int not_in();
	
	/* fprintf(stderr,"add ");	*/
	if(!flag_get(seen,rr+1,cc))	zero = add_in(r,c,rr+1,cc,zero,node_ct);
	if(!flag_get(seen,rr,cc+1))	zero = add_in(r,c,rr,cc+1,zero,node_ct);
	if(!flag_get(seen,rr-1,cc))	zero = add_in(r,c,rr-1,cc,zero,node_ct);
	if(!flag_get(seen,rr,cc-1))	zero = add_in(r,c,rr,cc-1,zero,node_ct);
	return (zero);
}

NODE *
add_in(r,c,rr,cc,zero,node_ct)
SHORT r, c,rr,cc;
NODE *zero;
int *node_ct;
{
	SHORT dor, doc;

	/* fprintf(stderr,"in(%d %d) ",rr,cc);	*/
	flag_set(seen,rr,cc);
	if (rr < minr) minr = rr;
	if (rr > maxr) maxr = rr;
	if (cc < minc) minc = cc;
	if (cc > maxc) maxc = cc;
	if(*node_ct == array_size)	{
		zero = (NODE *)realloc(zero,(array_size + AR_INCR) * 
					sizeof(NODE));
		array_size += AR_INCR;
	}
	dor = abs(rr - r);
	doc = abs(cc - c);
	zero[*node_ct].r = rr;
	zero[*node_ct].c = cc;
	zero[*node_ct].d = min(dor,doc) * 1.414 + abs(dor-doc);
	*node_ct = *node_ct + 1;
	return(zero);
}

set_seen(r,c)
SHORT r,c;
{
	flag_set(seen,r,c);
	if (r < minr) minr = r;
	if (r > maxr) maxr = r;
	if (c < minc) minc = c;
	if (c > maxc) maxc = c;
}
