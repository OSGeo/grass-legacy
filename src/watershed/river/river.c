/* program to map out drainage basin structure */
#define EXTERN
#define slopeput(row,col,value) segment_put(&slopeseg,&value,row,col)
#define wkput(row,col,value) segment_put(&wkseg,&value,row,col)
#define cumput(row,col,value) segment_put(&cumseg,&value,row,col)
#define lakeput(row,col,value) segment_put(&lakeseg,&value,row,col)
#define ptrput(row,col,value) segment_put(&ptrseg,&value,row,col)
#include "global.h"

CELL altget();
CELL cumget();
CELL ptrget(), lakeget();
char wkget();
char slopeget();

static struct Cell_head window;
static double H,V;
static double twoH, twoV;
static double Diag;
static int startrow,startcol;
static int cum_fd, cumcell_fd;
static int elev_fd;
static int lake_flag = 0;
static int verbose=0;
static int pitcnt,flag,maxpit,npits;
static double easting, northing;
static int expo[3][3] =
{
    {4, 3, 2},
    {5, 0, 1},
    {6, 7, 8}
};
static int drain[3][3] =
{
    {8, 7, 6},
    {1, 0, 5},
    {2, 3, 4}
};
double sqrt();
static char *lake_cats[] =
{
    "no data",
    "lake area",
    "redirected (toward pour point)",
    "pits",
    (char *)NULL
};

#define  POURPT struct pourpt
struct pourpt  
{
        int r,c;
        struct pourpt *nxt;
};

int dumpfd ;

char slope_val;
char wk_val;
CELL cell_val;

main(argc, argv) char *argv[];
{
    int temp,i,r,c,r1,c1,pcol,prow,dr,dc;
	int row, col;
    double maxd,del;
    int east_flag=0;
    int north_flag=0;
    char elev_name[40], *elev_mapset;
    char *cum_name;
    char *ptr_name;
	char cumcell_name[30], ptrcell_name[30];
    int ptr_fd, ptrcell_fd;
	char *alt_name;
	int alt_fd;
	char *wk_name;
	int wk_fd;
	char *slope_name;
	int slope_fd;
    char *lake_name;
	char lakecell_name[30];
    int lake_fd, lakecell_fd;
    int elev_flag=0;
    int cum_flag=0;
    int ptr_flag=0;
    FILE *pits_fd;
	CELL *cell_buf;
	CELL *cell_ptr;
	char *wk_buf, *slope_buf;
    struct Categories cats;
    struct Colors colr;
    int ncats;
    int maxpit_flag=0;
    char buf[100];
    int max, saverow, savecol;
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
        if (sscanf (argv[i], "elev=%[^\n]", elev_name) == 1)
        {
            if (elev_flag++)
			{
			    usage(argv[0]);
            }
            continue;
        }
        if (sscanf (argv[i], "accum=%[^\n]", cumcell_name) == 1)
        {
            if (cum_flag++)
			{
			    usage(argv[0]);
            }
            continue;
        }
        if (sscanf (argv[i], "drain=%[^\n]", ptrcell_name) == 1)
        {
            if (ptr_flag++)
			{
			    usage(argv[0]);
            }
            continue;
        }
        if (sscanf (argv[i], "lake=%[^\n]", lakecell_name) == 1)
        {
            if (lake_flag++)
			{
			    usage(argv[0]);
            }
            continue;
        }
        if (sscanf (argv[i], "east=%lf", &easting) == 1)
        {
            if (east_flag++)
			{
			    usage(argv[0]);
            }
            continue;
        }
        if (sscanf (argv[i], "north=%lf", &northing) == 1)
        {
            if (north_flag++)
			{
			    usage(argv[0]);
            }
            continue;
        }
        if (sscanf (argv[i], "pthres=%d", &maxpit) == 1)
        {
            if (maxpit_flag++)
			{
			    usage(argv[0]);
            }
            continue;
        }
        usage(argv[0]);
    }

    if ((!elev_flag) || (!cum_flag) || (!ptr_flag) ||
        (!east_flag) || (!north_flag) || (!maxpit_flag))
		{
            usage(argv[0]);
        }

    elev_mapset = G_find_file2("cell",elev_name,"");
    if (!elev_mapset)
    {
        sprintf(buf, "elevation file [%s] not found\n",
           elev_name);
        G_fatal_error (buf);
        exit(1);
    }

    G_get_set_window (&window);
    nrows = G_window_rows();
    ncols = G_window_cols();

    i = 1;
    if (nrows != (short) nrows)
    {
        fprintf (stderr, "# rows in window (%d) too large\n", nrows);
        i = 0;
    }
    if (ncols != (short) ncols)
    {
        fprintf (stderr, "# cols in window (%d) too large\n", ncols);
        i = 0;
    }
    if (i == 0) exit(1);

    H = window.ew_res;  /* Horizontal (east-west) run */
    V = window.ns_res;  /* Vertical (north-south) run */
    twoH = window.ew_res * 2;
    twoV = window.ns_res * 2;
    Diag = sqrt(H*H + V*V);   /* Diagonal run */

/*  open pits file (should be stored in pits directory under elev_name */

    pits_fd = G_fopen_old( "watershed/pits", elev_name, G_mapset());
    if (!pits_fd)
    {
        fprintf(stderr,"Pits file not found\n");
        fprintf(stderr," -run pits program for this elevation file\n");
        exit(2);
    }

    conv_coord();

/* initialize all map layers as segment files */
    elev_fd = G_open_cell_old(elev_name, elev_mapset);
    if (elev_fd < 0) exit(1);

    alt_name = G_tempfile();
    alt_fd = startseg(alt_name, &altseg, sizeof(CELL));

    cell_buf = G_allocate_cell_buf();

    for (row=0; row<nrows; row++)
    {
        G_get_map_row_nomask(elev_fd, cell_buf, row);
        segment_put_row(&altseg, cell_buf, row);
    }

    segment_flush(&altseg);

    G_close_cell(elev_fd);

    cum_name = G_tempfile();
    cum_fd = startseg(cum_name, &cumseg, sizeof(CELL));

    cell_ptr = cell_buf;
    for (col=0; col<ncols; col++)
        *cell_ptr++ = 1;

    for (row=0; row<nrows; row++)
    {
        segment_put_row(&cumseg, cell_buf, row);
    }

    segment_flush(&cumseg);

    wk_name = G_tempfile();
    wk_fd = startseg(wk_name, &wkseg, sizeof(char));

    wk_buf = G_calloc(ncols, sizeof(char));
    G_zero(wk_buf, ncols*sizeof(char));

    for (row=0; row<nrows; row++)
    {
        segment_put_row(&wkseg, wk_buf, row);
    }

    segment_flush(&wkseg);

    if (lake_flag)
    {
        lake_name = G_tempfile();
        lake_fd = startseg(lake_name, &lakeseg, sizeof(CELL));
    }

    ptr_name = G_tempfile();
    ptr_fd = startseg(ptr_name, &ptrseg, sizeof(CELL));

    G_zero_cell_buf(cell_buf);

    for (row=0; row<nrows; row++)
    {
        if (lake_flag) segment_put_row(&lakeseg, cell_buf, row);
        segment_put_row(&ptrseg, cell_buf, row);
    }

    if (lake_flag) segment_flush(&lakeseg);
    segment_flush(&ptrseg);

    slope_name = G_tempfile();
    slope_fd = startseg(slope_name, &slopeseg, sizeof(char));

    slope_buf = G_calloc(ncols, sizeof(char));
    G_zero(slope_buf, ncols*sizeof(char));

    for (row=0; row<nrows; row++)
    {
        segment_put_row(&slopeseg, slope_buf, row);
    }

    segment_flush(&slopeseg);
	free(slope_buf);
                

/**********************************************************************
 * calculate drainage direction and slope
 * NOTE: no longer calculating slope, as it is unneeded with the new
 * methods for finding the watershed
 * instead, the slope array is being used as a marker to denote lake
 * areas.  slope=0 means no lake, and slope=1 means lake
 *********************************************************************/

    if (verbose)
    {
        fprintf (stderr, "FIGURING SLOPE/ASPECT ...");
        fflush (stderr);

        percent (0, nrows, 10);
    }

    for(r = 1; r < nrows-1; r++)
    {
        if (verbose)
            percent (r, nrows, 10);
        for(c = 1; c < ncols-1; c++) 
        {
            maxd = 0.0;
            pcol = 1;
            prow = 1;
            for (dr = -1; dr <=1; dr++)
            {
                for (dc = -1; dc <= 1; dc++)
                {
                    del = (double)(altget(r,c) - altget(r+dr,c+dc));
                    if (dr != 0 && dc != 0) del=del/Diag;
                    else
                    {
                        if (dr != 0) del = del/V;
                        else del = del/H;
                    }
                    if (del > maxd)
                    {
                        maxd = del;
                        prow = 1 + dr;
                        pcol = 1 + dc;
                    }
                }
            }
            ptrput(r,c,expo[prow][pcol]);
        }
    }
    if (verbose)
    {
        percent (nrows, nrows, 10);

    }

    if (i = fread(&npits, sizeof(npits), 1, pits_fd) != 1)
         exit(5);

    if (verbose)
        printf(" number of pits is %d\n",npits);

    if (verbose)
    {
	printf("\n  Processing pits....");
    }
    for(r = 0; r < npits; r++)
    {
        if( (i = fread(&easting, sizeof(easting), 1, pits_fd)) != 1)
            exit(5);
        if( (i = fread(&northing, sizeof(northing), 1, pits_fd)) != 1)
            exit(5);

        r1 = ((window.north - northing) / window.ns_res);
        c1 = ((easting - window.west) / window.ew_res);

        if (r1 >= 0 && r1 < nrows && c1 >= 0 && c1 < ncols)
        {
            if (lake_flag)
			{
				cell_val = 3;
                if (lakeget(r1,c1) == 0) lakeput(r1,c1,cell_val);
			}
            if (r1 > 1 && c1 > 1 && r1 < nrows-2 && c1 < ncols-2 && 
              slopeget(r1,c1) != 1)
            {
                flag = 0;
                pitcnt = 0;
                filler((short)r1,(short)c1);
                if (flag == 0 && pitcnt != 1) lake(r1,c1);
            }
        }
    }

    fclose(pits_fd);

    if (lake_flag)
    {
        lakecell_fd = G_open_cell_new(lakecell_name);
        if (!lakecell_fd) exit(2);

        for (r=0; r<nrows; r++)
        {
            segment_get_row(&lakeseg, cell_buf, r);
            G_put_map_row(lakecell_fd, cell_buf, r);
        }

        G_close_cell(lakecell_fd);

        G_read_cats(lakecell_name,G_mapset(),&cats);

        G_set_cats_title("lakes", &cats);
        for (i=0; lake_cats[i] != NULL; i++)
            G_set_cat(i, lake_cats[i], &cats);

        G_write_cats(lakecell_name,&cats);
        G_free_cats (&cats);

        ncats = G_number_of_cats(lakecell_name, G_mapset());

        G_make_random_colors(&colr,0,ncats);
        G_write_colors(lakecell_name, G_mapset(), &colr);
        G_free_colors(&colr);
    }

/*    dumpfd = creat ("river.dump",0666); */

    if (verbose)
    {
	printf("\n  Marking watershed points...");
    }
    marker((short)startrow,(short)startcol);
    if (verbose)
    {
        printf(" \nCreating cell files...");
        fflush (stdout);
    }

    ptrcell_fd = G_open_cell_new(ptrcell_name);
    if (!ptrcell_fd) exit(2);

    for (r=0; r<nrows; r++)
    {
        segment_get_row(&ptrseg, cell_buf, r);
        G_put_map_row(ptrcell_fd, cell_buf, r);
    }

    G_close_cell(ptrcell_fd);


/**********************************************************************
 * write out marked network matrix
 **********************************************************************/

    cumcell_fd = G_open_cell_new(cumcell_name);
	segment_flush (&cumseg);
	segment_flush (&wkseg);
    for(r=0; r<nrows; r++)
    {
        segment_get_row(&cumseg, cell_buf, r);
        segment_get_row(&wkseg, wk_buf, r);

        for(c=0; c<ncols; c++)
            if(wk_buf[c] == 0) cell_buf[c] = 0;

        G_put_map_row(cumcell_fd, cell_buf, r);
    }
    G_close_cell(cumcell_fd);

    cumcell_fd = G_open_cell_old(cumcell_name, G_mapset());
	if (!cumcell_fd)
	{
		fprintf(stderr,"Error with accumulation map layer [%s]\n",
		  cumcell_name);
		fprintf(stderr,"...cannot save outlet\n");
		exit(-2);
	}

    max = -1;

    for (r=0; r<nrows; r++)
    {
        if (!G_get_map_row(cumcell_fd, cell_buf, r))
		{
			fprintf(stderr,"Error reading back accumulation map layer [%s]\n",
			  cumcell_name);
			exit(-2);
		}
        for (c=0; c<ncols; c++)
        {
			if (cell_buf[c] > max)
			{
                max = cell_buf[c];
                saverow = r;
                savecol = c;
            }
        }
    }
	G_close_cell(cumcell_fd);

    max_fd = G_fopen_new("watershed/max", cumcell_name);
    if (!max_fd)
    {
        fprintf(stderr,"error in saving maximum point\n");
        exit(2);
    }

    fprintf(max_fd,"%d %d\n", saverow, savecol);
    fclose(max_fd);

    exit(0);

}


/*********************************************************************
 * MARKER
 *********************************************************************/
marker(row,col)

    short row,col;

    {

    static int rvec[9] = { 0, 0, -1, -1, -1, 0, 1, 1, 1};
    static int cvec[9] = { 0, 1, 1, 0, -1, -1, -1, 0, 1};
    static int level = 0;
    static int max = -1;
    unsigned char dr,dc;


/*  the following section is a check for how many levels into
recursive calling this has gone.  This leaves a file lying around,
and so is being temporarily commented out.  The code is being left
in place in case it is needed later to check on problems with large
data files.
   ++level;
   if (level > max)
   {
       lseek (dumpfd, 0L, 0);
       max = level;
       write (dumpfd, &level, sizeof level);
    }
*/

/* just checking for grid edge */
    if (row <= 1 || col <= 1 || row >= nrows-2 || col >= ncols-2)  
        return;    

/* point is always marked as in */

    wk_val = 1;
    wkput(row,col,wk_val);

/* check 3x3 window */

    row--;
    col--;
    for (dr = 0; dr < 3; dr++)
    {
        for (dc = 0; dc < 3; dc++)
        {
            if (wkget(row,col) == 0 && ptrget(row,col) == drain[dr][dc])
                marker(row,col);
            else if(wkget(row,col) == 0 && slopeget(row,col) == 1)
            {
                ptrput(row,col,drain[dr][dc]);
                start_breath(row,col);
                marker(row,col);
            }
            col++;
        }
        row++;
        col -= 3;
    }
    col++;
    row -= 2;

    { register int r1, c1;

      r1 = row + rvec[ptrget(row,col)];
      c1 = col + cvec[ptrget(row,col)];
	  cell_val = cumget(r1,c1) + cumget(row,col);
      cumput(r1,c1,cell_val);
    }
    level--;
}


/**********************************************************************
 * PROCEDURE  FILLER
 **********************************************************************/

filler(row,col)
    short row,col;
{
    short r,c;
    unsigned char dc,dr;

    ++pitcnt;

/* just checking for grid edge */

    if ((row <= 1 || col <= 1 || row >= nrows-2 || col >= ncols-2)
          || slopeget(row,col) == 1)
    {
        flag = 1;
        return;
    }

/* point is always marked as in */
    wk_val = 1;
    wkput(row,col,wk_val);

/* check 3x3 window */
    for (dr = 0; dr < 3; dr++)
    {
        r = row + dr - 1;
        for (dc = 0; dc < 3; dc++)
        {
            c = col + dc - 1;
            if (wkget(r,c) == 0 && ((ptrget(r,c) == 0
                   && altget(r,c) == altget(row,col)) || 
                      ptrget(r,c) == drain[dr][dc]))
            {
                ptrput(r,c,drain[dr][dc]);
                filler(r,c);
            }
        }
    }
    flag = 0;
}

/********************************************************************
* PROCEDURE LAKE, by Chuck Ehlschlaeger
*********************************************************************/

lake(startr,startc)
{
    static int rvec[9] = {0, 0, 1, 1, 1, 0, -1, -1, -1};
    static int cvec[9] = {0, -1, -1, 0, 1, 1, 1, 0, -1};
    int r, c, pourht, checkht;
    POURPT *headp, *last, *new_pour_list(), *add_pour();
    int flag, endr, endc;

/* find pour height */
    pourht = 65535;

    endr = startr;
    endc = startc;
    flag = 1;

    while (flag)
    {
        flag = 0;
        startr = (startr<2)? (1):(startr-1);
        startc = (startc<2)? (1):(startc-1);
        endr = (endr>nrows-3)? (nrows-2):(endr+1);
        endc = (endc>ncols-3)? (ncols-2):(endc+1);
        for (r=startr; r<=endr; r++)
        {
            if (wkget(r,startc) == 1)
            {
                flag=1;
                if((altget(r,startc) < pourht) &&
                   (wkget(r-1,startc-1) + wkget(r-1,startc) +
                   wkget(r-1,startc+1) + wkget(r,startc-1) +
                   wkget(r,startc+1) + wkget(r+1,startc) +
                   wkget(r+1,startc-1) + wkget(r+1,startc+1))
                   != 8)
                        pourht = altget(r,startc);
            }
            if (wkget(r,endc) == 1)
            {
                flag=1;
                if((altget(r,endc) < pourht) &&
                   (wkget(r-1,endc-1) + wkget(r-1,endc) +
                   wkget(r-1,endc+1) + wkget(r,endc-1) +
                   wkget(r,endc+1) + wkget(r+1,endc) +
                   wkget(r+1,endc-1) + wkget(r+1,endc+1))
                   != 8)
                        pourht = altget(r,endc);
            }
        }
        for (c=startc; c<=endc; c++)
        {
            if (wkget(startr,c) == 1)
            {
                flag=1;
                if((altget(startr,c) < pourht) &&
                   (wkget(startr-1,c-1) + wkget(startr-1,c) +
                   wkget(startr-1,c+1) + wkget(startr,c-1) +
                   wkget(startr,c+1) + wkget(startr+1,c) +
                   wkget(startr+1,c-1) + wkget(startr+1,c+1))
                   != 8)
                        pourht = altget(startr,c);
            }
            if (wkget(endr,c) == 1)
            {
                flag=1;
                if((altget(endr,c) < pourht) &&
                   (wkget(endr-1,c-1) + wkget(endr-1,c) +
                   wkget(endr-1,c+1) + wkget(endr,c-1) +
                   wkget(endr,c+1) + wkget(endr+1,c) +
                   wkget(endr+1,c-1) + wkget(endr+1,c+1))
                   != 8)
                        pourht = altget(endr,c);
            }
        }
    }  /* end while loop */

/* find all lake points with alt <= pourht and set slope = -1.0 */
    for(r=startr; r<endr+1; r++)
    {
        for(c=startc; c<endc+1; c++)
        {
            if(wkget(r,c)==1 && altget(r,c)<=pourht+maxpit)
            {
				slope_val = 1;
                slopeput(r,c,slope_val);
                if(lake_flag && lakeget(r,c) == 0)
				{
					cell_val = 1;
                    lakeput(r,c,cell_val);
				}
            } 
			wk_val = 0;
            wkput(r,c,wk_val);
        }
    }
}

start_breath(r,c)
int r,c;
{
        POURPT *headp, *last, *new_pour_list();

        headp = last = new_pour_list(r,c);
        drain_basin(headp,last);
}

POURPT
*new_pour_list(r,c)
int r,c;
{
        POURPT *headp;

        headp = (POURPT *)G_malloc(sizeof(POURPT));
        headp->r = r;
        headp->c = c;
        headp->nxt =NULL;
        return(headp);
}

POURPT
*add_pour(last,r,c,fromr,fromc)
POURPT *last;
int r,c,fromr,fromc;
{
        last->nxt = (POURPT *)G_malloc(sizeof(POURPT));
        last = last->nxt;
        last->r = r;
        last->c = c;
        last->nxt = NULL;
        ptrput(r,c,expo[fromr-r+1][fromc-c+1]);
		wk_val = 1;
        wkput(r,c,wk_val);
        return(last);
}

drain_basin(headp,last)
POURPT *headp, *last;
{
    POURPT *killer, *do_br;
    int rr,cc;

    do_br = headp;
	wk_val = 1;
    wkput(do_br->r,do_br->c,wk_val);

    while(do_br != NULL)
    {
        for(rr= do_br->r-1; rr<=do_br->r+1; rr++)
        {
            for(cc= do_br->c-1; cc<=do_br->c+1; cc++)
            {
                if((rr != do_br->r || cc != do_br->c) &&
                  slopeget(rr,cc) == 1 && wkget(rr,cc) == 0)
                {
                    last = add_pour(last,rr,cc,do_br->r,do_br->c);
                }
            }
        }
        do_br = do_br->nxt;
    }
    while(headp != NULL)
    {
		wk_val = 0;
        wkput(headp->r,headp->c,wk_val);
        slopeput(headp->r,headp->c,wk_val);
        killer = headp;
        headp = headp->nxt;
        free(killer);
    }
}


/*********************************************************************
 *   CONVERT COORDINATES
 *********************************************************************/

 conv_coord()
 {
    char buf[100];

/*  Convert to row and column  */

    startrow = ( window.north - northing ) / window.ns_res ;

    startcol = ( easting - window.west ) / window.ew_res ;

    if (startrow < 0 || startcol < 0)
    {
        fprintf(stderr,"Illegal outlet coordinates -- outside current window");
        exit(-1);
    }

    if (startrow == 0 || startrow == 1)
        startrow = 2;
    if (startrow == nrows-1 || startrow == nrows-2)
        startrow = nrows - 3;
    if (startcol == 0 || startcol == 1)
        startcol = 2;
    if (startcol == ncols-1 || startcol == ncols-2)
        startcol = ncols - 3;

    if (verbose)
    {
        printf("outlet= %d %d\n",startrow,startcol);
        printf("number of rows = %d : number of cols = %d\n", nrows, ncols);
    }

}
