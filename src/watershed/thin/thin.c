/*  %W% %G% */
/* THIS VERSION OF THIN IS FOR DRAINAGE ACCUMULATION WITH ASPECT      */
/*====================================================================*/
#include "gis.h"

static int river_fd, thin_fd;
static char *river_mapset, *thin_mapset;
static char river_name[40],thin_name[40];
static CELL *nimg;
static CELL *nwork;
static int iters;
static int nrows, ncols;
static int maxrow, maxcol;
static int drain[3][3] = {
{8,7,6},
{1,0,5},
{2,3,4}
};

main (argc, argv) char *argv[];
{
    int  min;
    int row, col;
    struct Cell_head window;
    struct Colors colr;
    char buf[81];
    int i;
    int verbose=0;
    int river_flag=0;
    int thin_flag=0;
    int iters_flag=0;
    int min_flag=0;
    FILE *max_fd;

    G_gisinit (argv[0]);

/* parse command line */

    for (i=1; i < argc; i++)
    {
        if (strcmp (argv[i], "-v") == 0)
        {
            verbose = 1;
            continue;
        }
        if (sscanf (argv[i], "accum=%[^\n]", river_name) == 1)
        {
            if (river_flag++) usage(argv[0]);
            continue;
        }
        if (sscanf (argv[i], "thin=%[^\n]", thin_name) == 1)
        {
            if (thin_flag++) usage(argv[0]);
            continue;
        }
        if (sscanf (argv[i], "iters=%d", &iters) == 1)
        {
            if (iters_flag++) usage(argv[0]);
            continue;
        }
        if (sscanf (argv[i], "dthres=%d", &min) == 1)
        {
            if (min_flag++) usage(argv[0]);
            continue;
        }
        usage(argv[0]);
    }

    if ((!river_flag) || (!thin_flag) || (!iters_flag) || (!min_flag))
        usage(argv[0]);

    if (verbose)
       printf("\n Running...\n");

    river_mapset = G_find_file2("cell",river_name,"");
    if (!river_mapset)
    {
        sprintf(buf, "upslope accumulation file [%s] not found\n",
           river_name);
        G_fatal_error (buf);
        exit(1);
    }

    if (G_get_cellhd (river_name, river_mapset, &window) < 0)
    {
        printf(" can't read header for %s in %s\n",river_name, river_mapset);
        exit(2);
    }

    G_set_window(&window);

    nrows = G_window_rows();
    ncols = G_window_cols();

    river_fd = G_open_cell_old (river_name, river_mapset);
    if (river_fd < 0) exit(2);

    thin_fd = G_open_cell_new (thin_name);
    if (thin_fd < 0) exit(2);

    max_fd = G_fopen_old("watershed/max", river_name, river_mapset);
    if (!max_fd)
    {
	fprintf(stderr," unable to find stored outlet file\n");
	exit(2);
    }

    if (fscanf(max_fd, "%d %d", &maxrow, &maxcol) != 2)
    {
	fprintf(stderr,"error obtaining outlet point\n");
	exit(2);
    }

    fclose(max_fd);

    if (iters < 0)
    {
        fprintf(stderr,"Number of iterations must be a positive integer\n");
        usage(argv[0]);
    }
    if (verbose)
        printf ("%d iterations\n",iters);

    if (min < 0)
    {
        fprintf(stderr,"Drainage threshold must be a positive integer\n");
        usage(argv[0]);
    }
    if (verbose)
        printf ("threshold %d\n", min);

    nimg = (CELL *)G_malloc(nrows*ncols*sizeof(CELL));

    nwork = (CELL *)G_malloc(nrows*ncols*sizeof(CELL));

    for(row = 0; row < nrows; row++)
    {
        G_get_map_row( river_fd, nimg+row*ncols, row);
    }

    G_close_cell(river_fd);

    for (row=0; row<nrows; row++)
    {
        for(col = 0; col < ncols ; col++)
        {
            if ( nimg[row*ncols+col] < min ) nimg[row*ncols+col] = 0;
            else nimg[row*ncols+col] = 1;
        }
    } 

    linthn();

    for(row = 0; row < nrows; row++)
        G_put_map_row( thin_fd, nimg+row*ncols, row);

    G_close_cell(thin_fd);

    G_init_colors(&colr);
    G_set_color(0,128,128,128,&colr);
    G_set_color(1,0,0,255,&colr);
    G_write_colors (thin_name, G_mapset(), &colr);

    exit(0);
}

/* function linthin  */
linthn ()
{
    int i,j,r,c,p,nr1,nc1,idir;
    int n,nw,w,sw,s,se,e,ne,bn,bs,be,bw,nn,ns;
    int ptrcnt;
    int rowinc, colinc;

    nr1=nrows-1;
    nc1=ncols-1;


/* check and correct  diamond holes  */
    for (i=1; i<nr1; i++)
    {
        for (j=1; j<nc1; j++)
        {
            if (nimg[i*ncols+j] == 0
            && nimg[i*ncols+j+1]
            && nimg[(i-1)*ncols+j]
            && nimg[i*ncols+j-1]
            && nimg[(i+1)*ncols+j])
              nimg[i*ncols+j] = 1;
        }
    }

    for (p=1; p <= iters; p++)
    {
        for (r=0; r<nrows; r++)
            for (c=0; c<ncols; c++)
                nwork[r*ncols+c]=1;

/* form a zero valued buffer around the image circumference */
        for (i=0; i<nrows; i++)
        {
            nimg[i*ncols]=0;
            nwork[i*ncols]=0;
            nimg[i*ncols+nc1]=0;
            nwork[i*ncols+nc1]=0;
        }
        for (j=0; j<ncols; j++)
        {
            nimg[j]=0;
            nwork[j]=0;
            nimg[nr1*ncols+j]=0;
            nwork[nr1*ncols+j]=0;
        }

/* the following algorithm inspects the 3x3 window around a pixel and */
/* eliminates that pixel if it is simple and not an end point (using */
/* 8-connectivity). this is done, sequentially from the north, south, */
/* east and west.  */

        for (idir=1; idir<=4; idir++)
        {
            for (i=1; i<nr1; i++)
            {
                for (j=1; j<nc1; j++)
                {
                    if (nimg[i*ncols+j] == 0)
                    {
                        nwork[i*ncols+j] = 0;
                        goto contin ;
                    }

		    if ((i == maxrow) && (j == maxcol))
			goto contin ;

/*
                    ptrcnt = 0;
                    for (rowinc = -1; rowinc<=1; rowinc++)
		    {
                        for (colinc = -1; colinc<=1; colinc++)
			{
			    if (rowinc != 0 || colinc != 0)
			    {
			        if (aspect[(i+rowinc)*ncols+(j+colinc)] ==
			          drain[rowinc][colinc])
				    ptrcnt++;
			    }
			}
		    }
		    if (ptrcnt)
			goto contin;
*/

                    n = nimg[(i-1)*ncols+j];
                    nw = nimg[(i-1)*ncols+(j-1)];
                    w = nimg[i*ncols+(j-1)];
                    sw = nimg[(i+1)*ncols+(j-1)];
                    s = nimg[(i+1)*ncols+j];
                    se = nimg[(i+1)*ncols+(j+1)];
                    e = nimg[i*ncols+(j+1)];
                    ne = nimg[(i-1)*ncols+(j+1)];

                    if (n || nw || w || sw || s || se || e || ne)
                    {
                        bn = 1-n;
                        bs = 1-s;
                        be = 1-e;
                        bw = 1-w;
                        switch (idir)
                        {
                        case 1:
                          if (n != 0)
                            goto contin ;
                          nn = w*bs*e+bw*nw*bn+bn*ne*be+be*se*bs+bs*sw*bw;
                          if (nn == 0)
                            nwork[i*ncols+j] = 0;
                          break;
                        case 2:
                          if (w != 0)
                            goto contin ;
                          nw = n*be*s+bw*sw*bs+bs*se*be+be*ne*bn+bn*nw*bw;
                          if (nw == 0)
                            nwork[i*ncols+j] = 0;
                          break;
                        case 3:
                          if (e != 0)
                            goto contin ;
                          ne = n*bw*s+bn*ne*be+bs*se*be+bw*sw*bs+bw*nw*bn;
                          if (ne == 0)
                            nwork[i*ncols+j] = 0;
                          break;
                        case 4:
                          if (s !=0)
                            goto contin ;
                          ns = w*bn*e+bw*sw*bs+bs*se*be+be*ne*bn+bn*nw*bw;
                          if (ns == 0)
                              nwork[i*ncols+j] = 0;
                          goto contin ;
                          break;
                        }
                        if ((n+nw+w+sw+s+se+e+ne) == 1)
                            nwork[i*ncols+j] = 1;
                    }
                    else
                      nwork[i*ncols+j] = 0;
                contin: ;
                }
            }
/* after thinning the array, write the work array to the image array */
/* and return.  */
            for (i=0; i<nrows; i++)
                for (j=0; j<ncols; j++)
                    nimg[i*ncols+j] = nwork[i*ncols+j];
        }
    } 

}
