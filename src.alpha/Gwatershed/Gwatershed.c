/* program to map out drainage basin structure  */
/* this one uses the A * search algorithm   	*/
/* written by Chuck Ehlschlaeger        	*/
/* last modified 05/07/89           		*/
#include <math.h>
#include "gis.h"
#include "flag.h"
#include "cseg.h"

#define TSTR(x) /*   fprintf(stderr,"%s ",(x)) */
#define TS2(x)   /*  fprintf(stderr,"%s ",(x)) */
#define TNUM(x,y) /* fprintf(stderr,"%s %.2f  ",(x),(double)(y))   */
#define TN2(x,y)  /* fprintf(stderr,"%s %.2f  ",(x),(double)(y)) */

#define AR_SIZE     16
#define AR_INCR     16
#define SHORT       short
#define NOMASK      1
#define MIN_SLOPE   .00001
#define SROW        16
#define SCOL        (SROW)
#define RITE        1
#define LEFT        2
#define NEITHER     0

static SHORT        nrows, ncols;
static int          cells, rep;
static struct Cell_head window;
static double       diag;
static int	    bas_thres;
static FLAG         *seg, *worked, *in_list;
static CSEG         dis, alt, wat, asp, bas, haf;
static double       *slp;
static SHORT        sides;
static SHORT        drain[3][3] = 
{
    { 7,6,5 },
    { 8,0,4 },
    { 1,2,3 }
};

static SHORT        updrain[3][3] = 
{
    { 3,2,1 },
    { 4,0,8 },
    { 5,6,7 }
};

static SHORT        nextdr[8] = 
{
    1,-1,0,0,-1,1,1,-1 
};
static SHORT        nextdc[8] = 
{
    0,0,-1,1,1,-1,1,-1 
};
static char     wat_name[40], asp_name[40], arm_name[40], dis_name[40];
static char     ele_name[40], *ele_mapset;
static char     pit_name[40], *pit_mapset;
static char     run_name[40], *run_mapset;
static char	*this_mapset;
static char     seg_name[40];
static char     bas_name[40], haf_name[40], thr_name[8];
static char     ele_flag, pit_flag, run_flag, dis_flag;
static char     wat_flag, asp_flag, arm_flag, thr_flag;
static char     bas_flag, seg_flag, haf_flag;
static FILE     *fp;

#define POINT       struct points
POINT   
{
    SHORT       r, c, downr, downc;
};

#define  POURPTS    struct pourpts
POURPTS 
{
    CELL        elev;
    SHORT       numpts;
    SHORT       size;
    POINT       *pts;
    POURPTS     *nxt;
};

POURPTS         *all_pts, *wat_list, *put_perm_list();

main(argc, argv)
int argc;
char    *argv[];
{
    init_vars(argc,argv);
    do_astar();
    TS2("post_do_aster");
    do_cum();
    TS2("post_do_cum");
    if(!thr_flag)
    {
        close_maps();
    }
    else    
    {
        seg_step();
    }
    exit(0);
}

init_vars(argc,argv)
int argc;
char    *argv[];
{
    SHORT   r, c, dr, dc;
    CELL    *pit, *read_map();
    char    buf[100];
    int i, ct;
    CELL    value;
    POURPTS *add_pt();

    G_gisinit (argv[0]);
    ele_flag = wat_flag = asp_flag = pit_flag = thr_flag = run_flag = 0;
    bas_flag = seg_flag = haf_flag = arm_flag = dis_flag = 0;
    sides = 8;
    for (i = 1; i < argc; i++)
    {
        if     (sscanf(argv[i], "el=%[^\n]", ele_name) == 1) ele_flag++;
        else if(sscanf(argv[i], "ac=%[^\n]", wat_name) == 1) wat_flag++;
        else if(sscanf(argv[i], "dr=%[^\n]", asp_name) == 1) asp_flag++;
        else if(sscanf(argv[i], "pi=%[^\n]", pit_name) == 1) pit_flag++;
        else if(sscanf(argv[i], "th=%d",   &bas_thres) == 1) thr_flag++;
        else if(sscanf(argv[i], "ba=%[^\n]", bas_name) == 1) bas_flag++;
        else if(sscanf(argv[i], "se=%[^\n]", seg_name) == 1) seg_flag++;
        else if(sscanf(argv[i], "ha=%[^\n]", haf_name) == 1) haf_flag++;
        else if(sscanf(argv[i], "ov=%[^\n]", run_name) == 1) run_flag++;
        else if(sscanf(argv[i], "ar=%[^\n]", arm_name) == 1) arm_flag++;
        else if(sscanf(argv[i], "di=%[^\n]", dis_name) == 1) dis_flag++;
        else if(sscanf(argv[i], "-%hd", &sides) == 1) 
	{
	    if (sides != 4)
	    {
		usage(argv[0]);
	    }
	}
        else usage(argv[0]);
    }
    if((ele_flag != 1) || ((arm_flag == 1) && ((thr_flag != 1) || (haf_flag != 1))))
    {
        usage(argv[0]);
    }
    this_mapset = G_mapset();
    if(asp_flag)
    {
	if(G_legal_filename(asp_name) == -1)
	{
	    sprintf(buf, "drainage map layer name [%s] not legal for GRASS\n",asp_name);
	    G_fatal_error(buf);
	    exit(1);
	}
    }
    if(bas_flag)
    {
	if(G_legal_filename(bas_name) == -1)
	{
	    sprintf(buf, "basin map layer name [%s] not legal for GRASS\n",bas_name);
	    G_fatal_error(buf);
	    exit(1);
	}
    }
    if(seg_flag)
    {
	if(G_legal_filename(seg_name) == -1)
	{
	    sprintf(buf, "stream segment map layer name [%s] not legal for GRASS\n",seg_name);
	    G_fatal_error(buf);
	    exit(1);
	}
    }
    if(haf_flag)
    {
	if(G_legal_filename(haf_name) == -1)
	{
	    sprintf(buf, "half basin map layer name [%s] not legal for GRASS\n",haf_name);
	    G_fatal_error(buf);
	    exit(1);
	}
    }
    ele_mapset = G_find_cell2(ele_name, "");
    if (!ele_mapset)
    {
        sprintf(buf, "elevation file [%s] not found\n",
            ele_name);
        G_fatal_error (buf);
        exit(1);
    }
    if (pit_flag)
    {
        pit_mapset = G_find_cell2(pit_name, "");
        if (!pit_mapset)
        {
            sprintf(buf, "pits file [%s] not found\n",
                pit_name);
            G_fatal_error (buf);
            exit(1);
        }
    }
    G_get_set_window (&window);
    nrows = G_window_rows();
    ncols = G_window_cols();
    diag = sqrt(window.ew_res * window.ew_res + 
        window.ns_res * window.ns_res);
    cells = nrows * ncols;
    slp = (double *)G_malloc(cells * sizeof(double));
    for (ct=0; ct<cells; ct++)
    {
	slp[ct] = 0.0;
    }
    TN2("rows",nrows);
    TN2("columns",ncols);
    TN2("cells",cells);
    cseg_open(&alt, SROW, SCOL, nrows * 2 / SROW);
    cseg_read_map(&alt, ele_name);
    cseg_open(&wat, SROW, SCOL, nrows * 2 / SROW);
    if (run_flag)
    {
	cseg_read_map(&wat, run_name);
    }
    else 
    {
        for (c=0; c<ncols; c++)
        {
            cseg_value_to_row(&wat, c, 1);
        }
        for (r=0; r<nrows; r++)
        {
            cseg_row_to_seg (&wat, r);
        }
    }
    cseg_open(&asp, SROW, SCOL, 4);
    i = 0;
    in_list = flag_create(nrows,ncols);
    if (pit_flag)
    {
        pit = read_map (pit_name, pit_mapset);
        for (r=0; r<nrows; r++)
        {
            for (c=0; c<ncols; c++)
            {
                if(r==0 || c==0 || r==nrows-1 ||
                    c==ncols-1 ||pit[i])
                {
		    value = cseg_value_get(&wat, r, c);
		    if (value > 0)
		    {
			value = -value;
		    }
		    cseg_value_put(&wat, value, r, c);
		    if(r == 0) value = -2;
		    else if (c == 0) value = -4;
		    else if (r == nrows - 1) value = -6;
		    else if (c == ncols - 1) value = -8;
		    else value = -1;
		    cseg_value_put(&asp, value, r, c);
		    value = cseg_value_get(&alt, r, c);
                    all_pts = add_pt(r,c,-1,-1, value, value);
                }
                else
                {
		    cseg_value_put(&asp, 0, r, c);
                }
                i++;
            }
        }
        free(pit);
    }
    else 
    {
        for (r=0; r<nrows; r++)
        {
            for (c=0; c<ncols; c++)
            {
                if(r==0 || c==0 ||r==nrows-1 ||
                    c==ncols-1)
                {
		    value = cseg_value_get(&wat, r, c);
		    if (value > 0)
		    {
			value = -value;
		    }
		    cseg_value_put(&wat, value, r, c);
		    if(r == 0) value = -2;
		    else if (c == 0) value = -4;
		    else if (r == nrows - 1) value = -6;
		    else value = -8;
		    cseg_value_put(&asp, value, r, c);
		    value = cseg_value_get(&alt, r, c);
                    all_pts = add_pt(r,c,-1,-1, value, value);
                }
                else
                {
		    cseg_value_put(&asp, 0, r, c);
                }
                i++;
            }
        }
    }
    worked = flag_create(nrows,ncols);
    wat_list = NULL;
}

close_maps()
{
    struct Colors colors;
    int r, c;
    CELL min, max, value;

    TS2("close_maps");
    if(asp_flag)
    {
	cseg_to_map(&asp, asp_name);
	TSTR(" pre-G_in_c");
	G_init_colors(&colors);
	G_make_grey_scale(&colors, 1, 8);
	G_write_colors(asp_name, this_mapset, &colors);
	TSTR("pst-asp");
    }
    cseg_release(&asp);
    if(dis_flag)
    {
	TSTR("dis_flag");
	max = -1;
        cseg_open(&dis, 1, ncols, 1);
	TSTR("post cseg_open");
	for (r=0; r<nrows; r++)
	{
	    for (c=0; c<ncols; c++)
	    {
		if((value = cseg_value_get(&wat, r, c)) < 0)
		{
		    cseg_value_put(&dis, 0, r, c);
		}
		else if(value > 60)
		{
		    max = 60;
		    cseg_value_put(&dis, 60, r, c);
		}
		else
		{
		    if (value > max)
		    {
			max = value;
		    }
		    cseg_value_put(&dis, value, r, c);
		}
	    }
	}
	TSTR("post dis calc  ");
	cseg_to_map(&dis, dis_name);
	G_init_colors(&colors);
	G_make_rainbow_colors(&colors, 1, max);
	G_write_colors(dis_name, this_mapset, &colors);
	cseg_release(&dis);
	TSTR("post dis");
    }
    if(wat_flag)
    {
	cseg_to_map(&wat, wat_name);
    }
    cseg_release(&wat);
    G_free_colors(&colors);
}

POURPTS *
add_pt(r, c, downr, downc, ele, downe)
SHORT   r, c, downr, downc;
CELL    ele, downe;
{
    POURPTS *p, *new, *new_pt();
    POINT   *point;
    char    *G_malloc();
    SHORT   numpts;
    double  get_slope();

    slp[r*ncols+c] = get_slope(r, c, downr, downc, ele, downe);
    FLAG_SET(in_list,r,c);
    if (all_pts == NULL)
    {
        all_pts = (POURPTS *)G_malloc(sizeof(POURPTS));
        all_pts->elev = ele;
        all_pts->numpts = 1;
        point = all_pts->pts = (POINT *)G_malloc(sizeof(POINT) * AR_SIZE);
        point[0].r = r;
        point[0].c = c;
        point[0].downr = downr;
        point[0].downc = downc;
        all_pts->size = AR_SIZE;
        all_pts->nxt = NULL;
        return(all_pts);
    }
    p = all_pts;
    for(;;)
    {
        if (p->elev > ele)
        {
            new = (POURPTS *)G_malloc(sizeof(POURPTS));
            new->elev = p->elev;
            new->pts = p->pts;
            new->numpts = p->numpts;
            new->size = p->size;
            new->nxt = p->nxt;
            p->elev = ele;
            point = p->pts = (POINT *)G_malloc(sizeof(POINT) * AR_SIZE);
            point[0].r = r;
            point[0].c = c;
            point[0].downr = downr;
            point[0].downc = downc;
            p->size = AR_SIZE;
            p->numpts = 1;
            p->nxt = new;
            return(all_pts);
        }
        if (p->elev == ele)
        {
            if (p->numpts == p->size)
            {
                p->pts = (POINT *)realloc(p->pts,
                    (p->size + AR_INCR) * sizeof(POINT));
                p->size = p->size + AR_INCR;
            }
            point = p->pts;
            numpts = p->numpts;
            point[numpts].r = r;
            point[numpts].c = c;
            point[numpts].downr = downr;
            point[numpts].downc = downc;
            p->numpts = p->numpts + 1;
            return(all_pts);
        }
        if (p->nxt == NULL)
        {
            p->nxt = (POURPTS *)G_malloc(sizeof(POURPTS));
            p = p->nxt;
            p->nxt = NULL;
            p->elev = ele;
            p->numpts = 1;
            point = p->pts = (POINT *)G_malloc(AR_SIZE * sizeof(POINT));
            point[0].r = r;
            point[0].c = c;
            point[0].downr = downr;
            point[0].downc = downc;
            p->size = AR_SIZE;
            return(all_pts);
        }
        p = p->nxt;
    }
}

double
get_slope(r, c, downr, downc, ele, downe)
SHORT r, c, downr, downc;
CELL ele, downe;
{
    double slope;

    if(r == downr)
    {
	slope = (ele - downe) / window.ew_res;
    }
    else if(c == downc)
    {
	slope = (ele - downe) / window.ns_res;
    }
    else 
    {
	slope = (ele - downe) / diag;
    }
    if (slope < MIN_SLOPE)
	return(MIN_SLOPE);
    return(slope);
}

do_astar()
{
    POURPTS *doer;
    SHORT   upr, upc, r, c, ct_dir, ct_pt;
    int     count;
    CELL    value;
    double  slope;

    TS2("starting least cost optimal path algorithm");
    rep = 0;
    while (all_pts != NULL)
    {
        doer = all_pts;
        all_pts = all_pts->nxt;
        doer->nxt = wat_list;
        wat_list = doer;
        for (ct_pt = 0; ct_pt < doer->numpts; ct_pt++)
        {
            if (FLAG_GET(worked, doer->pts[ct_pt].r, doer->pts[ct_pt].c))
            {
                doer->pts[ct_pt].r = -10;
            }
            else 
            {
                FLAG_SET(worked, doer->pts[ct_pt].r, doer->pts[ct_pt].c);
            }
        }
        for (ct_pt = 0; ct_pt < doer->numpts; ct_pt++)
	{
            r = doer->pts[ct_pt].r;
            c = doer->pts[ct_pt].c;
            for (ct_dir = 0; ct_dir < sides; ct_dir++)
            {
                upr = r + nextdr[ct_dir];
                upc = c + nextdc[ct_dir];
                if(upr>=0 && upr<nrows && upc>=0 && upc<ncols)
                {
                    if (!(FLAG_GET(in_list,upr,upc)))
                    {
                        all_pts = add_pt(upr, upc, r, c, 
					cseg_value_get(&alt, upr, upc),
					cseg_value_get(&alt, r, c));
			cseg_value_put(&asp, drain[upr-r+1][upc-c+1], upr, upc);
                    }
                    else if (!(FLAG_GET(worked,upr,upc)))
                    {
			if (cseg_value_get(&asp, upr, upc) < 0)
                        {
			    cseg_value_put(&asp, drain[upr-r+1][upc-c+1], upr, upc);
			    value = cseg_value_get(&wat, r, c);
			    if(value > 0)
			    {
			        value = -value;
			    }
			    cseg_value_put(&wat, value, r, c);
			    value = cseg_value_get(&alt,upr,upc);
			    replace(upr, upc, r, c, value);
			    slp[upr*ncols+upc] = get_slope(upr, upc, r, c, value,
							cseg_value_get(&alt, r, c));
                        }
			else
			{
			    value = cseg_value_get(&alt, upr, upc);
			    slope = get_slope(upr, upc, r, c, value,
						cseg_value_get(&alt, r, c));
			    count = upr * ncols + upc;
			    if (slp[count] < slope)
			    {
				slp[count] = slope;
				cseg_value_put(&asp, drain[upr-r+1][upc-c+1], upr, upc);
				replace(upr, upc, r, c, value);
			    }
			}
                    }
                }
            }
        }
    }
    flag_destroy(worked);
    flag_destroy(in_list);
}

replace(upr, upc, r, c, ele)
SHORT upr, upc, r, c;
CELL ele;
{
    POURPTS *now;
    POINT *pts;
    int pt;

    rep++;
    now = all_pts;
    while (now != NULL)
    {
	if(now->elev == ele)
	{
	    pts = now->pts;
	    for (pt=0; pt<now->numpts; pt++)
	    {
		if (pts[pt].r == upr && pts[pt].c == upc)
		{
		    pts[pt].downr = r;
		    pts[pt].downc = c;
		    return;
		}
	    }
	}
	now = now->nxt;
    }
}

do_cum()
{
    SHORT   r, c, dr, dc, ct_pt;
    CELL    value, valued;
    POURPTS     *killer;
    POINT   *point;
    int     count, countd;

    while (wat_list != NULL)
    {
        killer = wat_list;
        wat_list = wat_list->nxt;
        point = killer->pts;
        for(ct_pt = 0; ct_pt < killer->numpts; ct_pt++)
        {
            if ((dr = point[ct_pt].downr) != -1)
            {
                r = point[ct_pt].r;
                c = point[ct_pt].c;
                count = r * ncols + c;
                dc = point[ct_pt].downc;
                countd = dr * ncols + dc;
		value = cseg_value_get(&wat, r, c);
		valued = cseg_value_get(&wat, dr, dc);
                if (value < 0 && valued < 0)
                    value += valued;
                else if (value > 0 && valued > 0)
                    value += valued;
                else if (value < 0)
                    value = value - valued;
                else
                    value = valued - value;
		cseg_value_put(&wat, value, dr, dc);
            }
        }
        free(killer);
    }
}

seg_step()
{
    init_array_seg();
    find_pourpts();
    close_array_seg();
}

init_array_seg()
{
    int     count;

    if(arm_flag)
    {
        fp = fopen(arm_name, "w");
    }
    seg = flag_create(nrows, ncols);
    cseg_open(&bas, SROW, SCOL, 4);
    cseg_open(&haf, SROW, SCOL, 4);
}

find_pourpts()
{
    int row, col;
    double easting, northing, stream_length;
    CELL old_elev, basin_num, value;

    TS2("find pourpts");
    basin_num = 0;
    for(row = 0; row < nrows; row++)
    {
        northing = window.north - (row + .5) * window.ns_res;
        for(col = 0; col < ncols; col++)
        {
	    if((value = cseg_value_get(&wat, row, col)) < 0)
	    {
		value = -value;
	    }
            if(cseg_value_get(&asp,row,col) < 0 && 
	       value >= bas_thres)
            {
                basin_num += 2;
                old_elev = cseg_value_get(&alt, row, col);
                if (arm_flag)
                {
                    easting = window.west + (col + .5) * window.ew_res;
                    fprintf(fp,"%5d drains into %5d at %3d %3d %lf %lf",
                        (int) basin_num, 0, row, col, easting, northing);
                }
                if(col == 0 || col == ncols -1)
                {
                    stream_length = .5 * window.ew_res;
                }
                else if (row == 0 || row == nrows - 1)
                {
                    stream_length = .5 * window.ns_res;
                }
                else    
                {
                    stream_length = 0.0;
                }
                basin_num=def_basin(row,col,basin_num,stream_length,old_elev);
            }
        }
    }
}

CELL
def_basin(row,col,basin_num,stream_length,old_elev)
double stream_length;
int row, col;
CELL basin_num, old_elev;
{
    int r, rr, c, cc, ct, new_r[9], new_c[9];
    double easting, northing, slope;
    CELL value, new_elev, old_basin;
    SHORT updir, oldupdir, downdir, riteflag, leftflag, thisdir;
    double junk;
    CELL jnk2;

    for(;;)
    {
        cseg_value_put(&bas, basin_num, row, col);
        FLAG_SET(seg, row, col);
        ct = 0;
        for (r=row-1, rr=0; rr<3; r++, rr++)
        {
            for (c=col-1, cc=0; cc<3; c++, cc++)
            {
                if(r >= 0 && c >= 0 && r < nrows && c < ncols)
                {
		    if((value = cseg_value_get(&asp, r, c)) < 0)
			value = -value;
                    if (value  == drain[rr][cc])
                    {
		        if((value = cseg_value_get(&wat, r, c)) < 0)
				value = -value;
                        if(value >= bas_thres)
                        {
                            new_r[++ct] = r;
                            new_c[ct] = c;
                        }
                    }
                }
            }
        }
        if (ct == 0)
        {
            no_stream(row,col,basin_num,stream_length,old_elev);
            return(basin_num);
        }
        if (ct >= 2)   
        {
	    basin_num = split_stream(row, col, new_r, new_c, ct,
			    basin_num, stream_length, old_elev);
            return(basin_num);
        }
	oldupdir = drain[row-new_r[1]+1][col-new_c[1]+1]; 
	if ((downdir = cseg_value_get(&asp, row, col)) < 0)
	{
	    downdir = -downdir;
	}
	riteflag = leftflag = 0;
	for (r=row-1, rr=0; rr<3; r++, rr++)
	{
	    for (c=col-1, cc=0; cc<3; c++, cc++)
	    {
		if(r >= 0 && c >= 0 && r < nrows && c < ncols &&
		   cseg_value_get(&asp, r, c)  == drain[rr][cc])
		{
		    thisdir = updrain[rr][cc];
		    switch (haf_basin_side(oldupdir, downdir, thisdir))
		    {
			case LEFT:
			    overland_cells(r,c, basin_num, basin_num-1, &slope, &new_elev);
			    leftflag++;
			    break;
			case RITE:
			    overland_cells(r,c, basin_num, basin_num, &slope, &new_elev);
			    riteflag++;
			    break;
		    }
		}
	    }
	}
	if(leftflag > riteflag)
	{
	    cseg_value_put(&haf, basin_num - 1, row, col);
	}
	else
	{
	    cseg_value_put(&haf, basin_num, row, col);
	}
	if(new_r[1] != row && new_c[1] != col)
	{
	    stream_length += diag;
	}
	else if (new_r[1] != row)
	{
	    stream_length += window.ns_res;
	}
	else    
	{
	    stream_length += window.ew_res;
	}
	row = new_r[1];
	col = new_c[1];
    }
}
    
CELL
split_stream(row, col, new_r, new_c, ct, basin_num, stream_length, old_elev)
int row, col, new_r[], new_c[], ct;
CELL basin_num, old_elev;
double stream_length;
{
    CELL old_basin, new_elev;
    double slope, easting, northing;
    SHORT doit, ctr, updir, oldupdir, splitdir[9], downdir, thisdir, leftflag, riteflag;
    int r, c, rr, cc;

    TSTR("split stream");
    for(ctr=1; ctr<=ct; ctr++)
    {
	splitdir[ctr] = drain[row-new_r[ctr]+1][col-new_c[ctr]+1];
    }
    oldupdir = splitdir[1];
    if((downdir = cseg_value_get(&asp, row, col)) < 0)
    {
	downdir = -downdir;
    }
    riteflag = leftflag = 0;
    for (r=row-1, rr=0; rr<3; r++, rr++)
    {
        for (c=col-1, cc=0; cc<3; c++, cc++)
        {
            if(r >= 0 && c >= 0 && r < nrows && c < ncols &&
               cseg_value_get(&asp, r, c)  == drain[rr][cc])
            {
		doit = 1;
		thisdir = updrain[rr][cc];
		for(ctr=1; ctr<=ct; ctr++)
		{
		    if(thisdir == splitdir[ctr])
		    {
			doit = 0;
			ctr = ct;
		    }
		}
		if(doit)
		{
		    thisdir = updrain[rr][cc];
		    switch (haf_basin_side(oldupdir, downdir, thisdir))
		    {
			case LEFT:
			    overland_cells(r,c, basin_num, basin_num-1, &slope, &new_elev);
			    leftflag++;
			    break;
			case RITE:
			    overland_cells(r,c, basin_num, basin_num, &slope, &new_elev);
			    riteflag++;
			    break;
		    }
		}
            }
        }
    }
    if(leftflag >= riteflag)
    {
	cseg_value_put(&haf, basin_num - 1, row, col);
    }
    else
    {
	cseg_value_put(&haf, basin_num, row, col);
    }
    old_basin = basin_num;
    new_elev = cseg_value_get(&alt, row, col);
    if((slope = (new_elev - old_elev) / stream_length) < MIN_SLOPE)
    {
        slope = MIN_SLOPE;
    }
    if (arm_flag)
    {
        fprintf(fp," %lf %lf\n", slope, stream_length);
    }
    for (r=1; r<=ct; r++)
    {
        basin_num += 2;
        easting = window.west + (new_c[r] + .5) * window.ew_res;
        northing = window.north - (new_r[r] + .5) * window.ns_res;
        if (arm_flag)
        {
            fprintf(fp, "%5d drains into %5d at %3d %3d %lf %lf",
                (int) basin_num, old_basin, new_r[r],new_c[r],easting,northing);
        }
        if(new_r[r] != row && new_c[r] != col)
        {
            basin_num = def_basin(new_r[r],new_c[r], basin_num, diag, new_elev);
        }
        else if (new_r[r] != row)
        {
            basin_num = def_basin(new_r[r],new_c[r], basin_num, window.ns_res, new_elev);
        }
        else    
        {
            basin_num = def_basin(new_r[r],new_c[r], basin_num, window.ew_res, new_elev);
        }
    }
    return(basin_num);
}

overland_cells(row,col,basin_num, haf_num, length_slope, hih_ele)
int row, col;
CELL basin_num, haf_num, *hih_ele;
double *length_slope;
{
    int r, rr, c, cc;
    double new_length, new_max_len;
    CELL new_ele, new_max_ele;

    new_max_len = *length_slope;
    cseg_value_put(&bas, basin_num, row, col);
    cseg_value_put(&haf, haf_num, row, col);
    new_max_ele = -1000;
    for(r=row-1, rr=0; r<=row+1; r++, rr++)
    {
        for(c=col-1, cc=0; c<=col+1; c++, cc++)
        {
            if (r>=0 && c>=0 && r<nrows && c<ncols)
            {
                if(cseg_value_get(&asp, r, c) == drain[rr][cc])
                {
		    if (r != row && c != col)
		    {
                        new_length = *length_slope + diag;
			overland_cells(r,c,basin_num, haf_num, &new_length, &new_ele);
		    }
		    else if (r != row)
		    {
                        new_length = *length_slope + window.ns_res;
			overland_cells(r,c,basin_num, haf_num, &new_length, &new_ele);
		    }
		    else
		    {
                        new_length = *length_slope + window.ew_res;
			overland_cells(r,c,basin_num, haf_num, &new_length, &new_ele);
		    }
		    if (new_length > new_max_len)
		    {
			new_max_len = new_length;
			new_max_ele = new_ele;
		    }
                }
            }
        }
    }
    *length_slope = new_max_len;
    if (new_max_ele == -1000)
    {
	*hih_ele = cseg_value_get(&alt, row, col);
    }
    else
    {
	*hih_ele = new_max_ele;
    }
}

no_stream(row,col,basin_num,stream_length,old_elev)
double stream_length;
int row, col;
CELL basin_num,old_elev;
{
    int r, rr, c, cc, uprow, upcol;
    double slope, total_length, new_length;
    CELL max_drain, value, hih_ele, new_ele;
    SHORT oldupdir, updir, downdir, riteflag, leftflag, thisdir;

    max_drain = 0;
    while (max_drain > -1)
    {
        max_drain = -1;
        total_length = stream_length;
        for(r=row-1, rr=0; r<=row+1; r++, rr++)
        {
            for(c=col-1, cc=0; c<=col+1; c++, cc++)
            {
                if (r>=0 && c>=0 && r<nrows && c<ncols)
                {
                    if(cseg_value_get(&asp, r, c) == drain[rr][cc])
                    {
		        if((value = cseg_value_get(&wat, r, c)) < 0)
			{
			    value = -value;
			}
                        if (value > max_drain)
                        {
                            uprow = r;
                            upcol = c;
                            max_drain = value;
                        }
                    }
                }
            }
        }
        if (max_drain > -1)
        {
            hih_ele = -10000;
            oldupdir = drain[row-uprow+1][col-upcol+1];
            if ((downdir = cseg_value_get(&asp, row, col)) < 0)
	    {
		downdir = -downdir;
	    }
            riteflag = leftflag = 0;
            for (r=row-1, rr=0; rr<3; r++, rr++)
            {
                for (c=col-1, cc=0; cc<3; c++, cc++)
                {
                    if(r >= 0 && c >= 0 && r < nrows && c < ncols &&
                       cseg_value_get(&asp, r, c)  == drain[rr][cc])
                    {
			thisdir = updrain[rr][cc];
		        if (r != row && c != col)
		        {
			    new_length = stream_length + diag;
		        }
		        else if (r != row)
		        {
			    new_length = stream_length + window.ns_res;
		        }
		        else
		        {
			    new_length = stream_length + window.ew_res;
		        }
		        if(haf_basin_side(oldupdir, downdir, thisdir) == RITE)
		        {
		            overland_cells(r, c, basin_num, basin_num, &new_length, &new_ele);
		            riteflag++;
		        }
		        else
		        {
		            overland_cells(r, c, basin_num, basin_num-1, &new_length, &new_ele);
		            leftflag++;
	                }
		        if (total_length < new_length)
		        {
			    total_length = new_length;
			    hih_ele = new_ele;
		        }
                    }
                }
            }
            if(leftflag >= riteflag)
            {
	        cseg_value_put(&haf, basin_num - 1, row, col);
            }
            else
            {
	        cseg_value_put(&haf, basin_num, row, col);
            }
        }
        else
        {
            hih_ele = cseg_value_get(&alt, row, col);
            if((slope = (hih_ele - old_elev) / stream_length) < MIN_SLOPE)
            {
                slope = MIN_SLOPE;
            }
            if (arm_flag)
            {
                fprintf(fp," %lf %lf\n",slope,stream_length);
            }
        }
	row = uprow;
	col = upcol;
    }
}
    
close_array_seg()
{
    struct Colors colors;
    int incr, max, red, green, blue, rd, gr, bl, flag;
    int c, r, map_fd;
    CELL *cellrow, value;
    CSEG *theseg;

    close_maps();
    if (seg_flag || bas_flag || haf_flag)
    {
	if(seg_flag)
	{
	    theseg = &bas;
	}
	else if(bas_flag)
	{
	    theseg = &bas;
	}
	else
	{ 
	    theseg = &haf;
	}
	max = -9;
	for (r=0; r<nrows; r++)
	{
	    for (c=0; c<ncols; c++)
	    {
		if((value = cseg_value_get(&haf, r, c)) > max)
		{
		    max = value;
		}
	    }
	}
	G_init_colors(&colors);
	G_make_random_colors(&colors, 1, max);
	G_set_color(0, 0, 0, 0, &colors);
	r = 1;
	incr = 0;
	while(incr >= 0)
	{
	    for (gr=130+incr; gr<=255; gr += 20)
	    {
	        for (rd=90+incr; rd<=255; rd += 30)
	        {
		    for (bl=90+incr; bl<=255; bl += 40)
		    {
		        flag = 1;
		        while (flag)
		        {
	                    G_get_color(r,&red,&green,&blue, &colors);
			    if((blue*.11 + red*.30 + green*.59) < 100)
		            {
			        G_set_color(r, rd, gr, bl, &colors);
			        flag = 0;
		            }
			    if (++r > max)
			    {
			        gr = rd = bl = 300;
				flag = 0;
			        incr = -1;
			    }
		        }
		    }
	        }
	    }
	    if (incr >= 0)
	    {
		incr += 15;
		if (incr > 120)
		{
		    incr = 7;
		}
	    }
	}
    }
    if (seg_flag)
    {
	cellrow = (CELL *)G_malloc(ncols * sizeof(CELL));
        map_fd = G_open_cell_new(seg_name);
        for (r=0; r<nrows; r++)
        {
	    for (c=0; c<ncols; c++)
	    {
		if (flag_get(seg, r, c))
		{
		    cellrow[c] = cseg_value_get(&bas, r, c);
		}
		else
		{
		    cellrow[c] = 0;
		}
	    }
            G_put_map_row(map_fd, cellrow, r);
        }
	free(cellrow);
        G_close_cell(map_fd);
	G_write_colors(seg_name, this_mapset, &colors);
    }
    flag_destroy(seg);
    if (bas_flag)
    {
	cseg_to_map(&bas, bas_name);
	G_write_colors(bas_name, this_mapset, &colors);
    }
    if (haf_flag)
    {
	cseg_to_map(&haf, haf_name);
	G_write_colors(haf_name, this_mapset, &colors);
    }
    if(seg_flag || bas_flag || haf_flag)
    {
	G_free_colors(&colors);
    }
    cseg_release(&haf);
    cseg_release(&bas);
    if(arm_flag)
    {
        fclose(fp);
    }
}

CELL *
read_map(name, mapset)
char *name;
char *mapset;
{
    int fd;
    CELL *map;
    int row;
    int G_get_map_row_nomask();
    char buf[100];

    /* allocate entire map */
    map = (CELL *)G_malloc(nrows*ncols*sizeof(CELL));

    if ((fd = G_open_cell_old (name, mapset)) < 0)
    {
	sprintf(buf, "unable to open file [%s]\n", name);
	G_fatal_error(buf);
    }

    /* read the map */
    for (row = 0; row < nrows; row++)
    {
        if (G_get_map_row_nomask(fd, map+row*ncols, row) < 0)
        {
	    sprintf(buf, "error reading file [%s]\n", name);
	    G_fatal_error(buf);
        }
    }
    G_close_cell (fd);
    return(map);
}

haf_basin_side(updir,downdir,thisdir)
SHORT updir, downdir, thisdir;
{
	SHORT newup, newthis;

      newup = updir - downdir;
      if (newup < 0) newup += 8;
      newthis = thisdir - downdir;
      if (newthis < 0) newthis += 8;
      if (newthis < newup) return(LEFT);
      if (newthis > newup) return(RITE);
      return(NEITHER);
}
