/*  %W% %G% */

/* program to map out drainage basin structure */

#include "gis.h"

    static char ptr_name[100], thin_name[100];
    static char basin_name[100], order_name[40];
    static char *mapset;
    static char *basin_mapset;
    static char *ptr_mapset;
    static char *thin_mapset;
    static CELL *ptr;    /* ptr data */
    static CELL *basin;    /* subwatershed numbering */
    static FILE *node_fd;
    static int ptr_fd;
    static int basin_fd;
    static int nnode;
    static int nrows, ncols;
    static struct Cell_head window;
    struct topo
    {
        int code;
        struct topo *pre;
        struct topo *post;
        int r1;
        int c1;
        int rv;
        int cv;
        int array_num;
        int seg_num;
        int type;
    };
    static struct topo *node;
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


main(argc, argv) char *argv[];

{
    int temp,i,r,c,r1,c1,pcol,prow,dr,dc;
    int ncats;
    struct Colors colors;
    int verbose=1;
    int ptr_flag = 0;
    int thin_flag = 0;
    int basin_flag = 0;
    char buf[100];

    G_gisinit(argv[0]);

/* parse command line */

    for (i=1; i < argc; i++)
    {
        if (strcmp (argv[i], "-v") == 0)
	{
	    verbose = 0;
	    continue;
	}
	if (sscanf (argv[i], "drain=%[^\n]", ptr_name) == 1)
	{
	    if (ptr_flag++) usage(argv[0]);
	    continue;
	}
	if (sscanf (argv[i], "stream=%[^\n]", thin_name) == 1)
	{
	    if (thin_flag++) usage(argv[0]);
	    continue;
	}
	if (sscanf (argv[i], "basin=%[^\n]", basin_name) == 1)
	{
	    if (basin_flag++) usage(argv[0]);
	    continue;
	}
	usage(argv[0]);
    }

    if ((!ptr_flag) || (!thin_flag) || (!basin_flag))
	usage(argv[0]);

    if (verbose)
       printf("\n Running...\n");

    ptr_mapset = G_find_file2("cell",ptr_name,"");
    if (!ptr_mapset)
    {
	sprintf(buf, "drainage direction file [%s] not found\n",
           ptr_name);
        G_fatal_error (buf);
        exit(1);
    }

    thin_mapset = G_find_file2("cell",thin_name,"");
    if (!thin_mapset)
    {
	sprintf(buf, "stream network file [%s] not found\n",
           thin_name);
        G_fatal_error (buf);
        exit(1);
    }

    if (G_get_cellhd(thin_name,thin_mapset,&window) < 0)
    {
	sprintf(buf, "error in obtaining cell header file for %s\n",
           thin_name);
        G_fatal_error (buf);
        exit(1);
    }

    G_set_window(&window);

    nrows = G_window_rows();
    ncols = G_window_cols();

    basin_mapset = G_find_file("watershed/nodes",thin_name,"");
    if (!basin_mapset)
    {
        sprintf(buf,"unable to find nodes file\n");
        G_fatal_error (buf);
        exit(1);
    }

    node_fd = G_fopen_old("watershed/nodes",thin_name,basin_mapset);
    if (!node_fd) exit(1);

    basin_fd = G_open_cell_new(basin_name);
    if (basin_fd < 0)
        exit(2);

    ptr_fd = G_open_cell_old(ptr_name,ptr_mapset);
    if (ptr_fd < 0) exit(2);

/*******************************************************************/
/*
/*      Read pointer data
/*
/*******************************************************************/

    ptr = (CELL *)G_malloc(nrows*ncols*sizeof(CELL));

    for(r=0; r<nrows; r++)
        G_get_map_row(ptr_fd, ptr+r*ncols, r);

    G_close_cell (ptr_fd);

    basin = (CELL *)G_malloc(nrows*ncols*sizeof(CELL));

    for (r=0; r<nrows; r++)
    for (c=0; c<ncols; c++)
        basin[r*ncols+c] = 0;

    if (fscanf(node_fd,"%d\n",&nnode) != 1)
    {
        fprintf(stderr," error in reading number of nodes\n");
        exit(5);
    }

    node = (struct topo *)G_calloc(nnode+1,sizeof(struct topo));

    node[0].code = 5;
    for(i = 1; i <= nnode; i++)
    {
        if (fscanf(node_fd,"%d %d %d %d %d %d %d %*lf\n",&node[i].array_num,
            &node[i].r1,&node[i].c1,&node[i].seg_num,&node[i].rv,
            &node[i].cv,&node[i].code) != 7)
        {
            fprintf(stderr," error reading nodes file\n");
            exit(5);
        }
        node[i].type = node[i].code;
        if (verbose)
	    printf("num= %d  r1= %d  c1= %d  code= %d\n",node[i].seg_num,
               node[i].r1,node[i].c1,node[i].code);
        node[i].post = &node[i+1];
        node[i].pre = &node[i-1];
    }

    fclose(node_fd);

    i = 1;
    topol(i);

/**********************************************************************/
/*   write out marked network matrix */
/**********************************************************************/
    for (r=0; r<nrows; r++)
        G_put_map_row(basin_fd, basin+r*ncols, r);

    G_close_cell(basin_fd);

    ncats = G_number_of_cats(basin_name,G_mapset());

    if (G_make_random_colors(&colors,0,ncats) == -1)
    {
        fprintf(stderr,"error in getting color table for basin\n");
        exit(-2);
    }
    G_write_colors(basin_name,G_mapset(),&colors);

    exit(0);

}
/********************************************************************/
/* TOPOL  recursively works through topologic code, finding, marking*/
/*        and pruning first order streams, until the root is found. */
/**********************************************************************/

topol(i)

{
    if (i==0 || i > nnode)    return;

    if (node[i].code == 0 && node[i].pre->code != 0)
/*    if (node[i].code == 0 && node[i].pre->code != 0 && i != 1) */
    {
        node[i].pre->code--;
        node[i].pre->post = node[i].post;
        node[i].post->pre = node[i].pre;
        marker(node[i].r1,node[i].c1,node[i].seg_num,node[i].array_num);
        i = node[i].pre->array_num;
    }
    else
        i = node[i].post->array_num;

    topol(i);    
}


/*********************************************************************/
/* MARKER                                                            */
/*********************************************************************/

marker(row,col,seg_num,array_num)

    int row,col,seg_num,array_num;

{
    static int rvec[9] = { 0, 0, -1, -1, -1, 0, 1, 1, 1};
    static int cvec[9] = { 0, 1, 1, 0, -1, -1, -1, 0, 1};
    int r,c,dc,dr,r1,c1;

/* just checking for grid edge */
    if (row <= 1 || col <= 1 || row >= nrows-2 || col >= ncols-2)
        return;    

    if (row==node[array_num].rv && col==node[array_num].cv &&
      node[array_num].type != 0)
        return;

/* point is always marked as in */
    basin[row*ncols+col] = seg_num;

/* check 3x3 window */
    for (dr = -1; dr <= 1; dr++)
    {
        r = row + dr;
        for (dc = -1; dc <=1; dc++)
        {
            c = col + dc;
            if (basin[r*ncols+c] == 0 && 
                ptr[r*ncols+c] == drain[dr+1][dc+1])
            {
                marker(r,c,seg_num,array_num);
            }
        }
    }

    r1 = row + rvec[ptr[row*ncols+col]];
    c1 = col + cvec[ptr[row*ncols+col]];

}
