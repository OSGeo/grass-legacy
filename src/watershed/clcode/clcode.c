/*  %W% %G% */
/* program for climbing and vector-coding of stream network */
/* requires as input an integer matrix containing binary    */
/* network image.  values are replaced by integer descriptor*/
/* of each link and written back into the file.  input      */
/* starting pixel coordinates and coordinates of adjacent  */
/* pixel to set up initial pointer.                         */
/*--------------------------------------------------------------------*/
/*====================================================================*/

#include "gis.h"

#define fabs(x) ((x)<0?-(x):(x))

    static char *mapset;
    static char link_name[40], node_name[40];
    static char thin_name[100], *thin_mapset;
    static int link_fd, thinriver_fd;
    static int link_flag=0;
    static FILE *node_fd;
    static FILE *renum_fd;
    static FILE *drain_fd;
    static CELL *nimg;
    static int nrows,ncols;
    static int nodecnt,flag;
    static int startr,startc,startrpt,startcpt;
    static struct Cell_head window;
    static double chlength;
    static int startflag;
    static double diag;
    double sqrt();
    static int watshed_num;
    static int expo[3][3] =
    {
	{4, 3, 2},
	{5, 0, 1},
	{6, 7, 8}
    };


main(argc, argv) char *argv[];
{
    int temp,r,c,ro,co;
    int i;
    int ncats;
    char buf[300];
    int river_flag=0;
    int thin_flag=0;
    int ptr_flag=0;
    char ptr_name[40], *ptr_mapset;
    int ptr_fd;
    FILE *max_fd;
    int ptr_val;

    struct Colors colors;
    typedef struct
    {
        int num;
        int r1;
        int c1;
        int rend;
        int cend;
        int code;
        double chlength;
    } topo;

    topo *node;

    int *renum;
    int oldnum, newnum;

    typedef struct
    {
        int numfrom;
        int numinto;
    } drainage;

    drainage *drain;
    char river_name[100];
    char *river_mapset;
    int verbose=0;
    CELL *ptr_row;


    G_gisinit("CLCODE");

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
	if (sscanf (argv[i], "stream=%[^\n]", thin_name) == 1)
	{
	    if (thin_flag++) usage(argv[0]);
	    continue;
	}
	if (sscanf (argv[i], "drain=%[^\n]", ptr_name) == 1)
	{
	    if (ptr_flag++) usage(argv[0]);
	    continue;
	}
	if (sscanf (argv[i], "out=%[^\n]", link_name) == 1)
	{
	    if (link_flag++) usage(argv[0]);
	    continue;
	}
	usage(argv[0]);
    }

    if ((!river_flag) || (!thin_flag) || (!ptr_flag))
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

    ptr_mapset = G_find_file2("cell",ptr_name,"");
    if (!ptr_mapset)
    {
	sprintf(buf, "drainage direction file [%s] not found\n",
            ptr_name);
        G_fatal_error(buf);
    }

    thin_mapset = G_find_file2("cell",thin_name,"");
    if (!thin_mapset)
    {
	sprintf(buf, "stream network file [%s] not found\n",
            thin_name);
        G_fatal_error(buf);
    }

    if (G_get_cellhd(thin_name,thin_mapset,&window) < 0)
    {
        sprintf(buf,"problem with cell header for %s\n",thin_name);
        G_fatal_error(buf);
    }
        
    G_set_window (&window);

    nrows = G_window_rows();
    ncols = G_window_cols();

    diag = sqrt(window.ns_res*window.ns_res + window.ew_res*window.ew_res);

    thinriver_fd = G_open_cell_old(thin_name,thin_mapset);
    if (thinriver_fd < 0)
        exit(2);

    node_fd = G_fopen_new("watershed/nodes",thin_name,G_mapset());
    if (!node_fd) exit(2);

    renum_fd = G_fopen_new("watershed/renum",thin_name,G_mapset());
    if (!renum_fd) exit(2);

    drain_fd = G_fopen_new("watershed/drain",thin_name,G_mapset());
    if (!drain_fd) exit(2);

    if (link_flag)
    {
	link_fd = G_open_cell_new(link_name);
	if (link_fd < 0)
	    exit(2);
    }
/* read in network image */
    nimg = (CELL *)G_malloc(nrows*ncols*sizeof(CELL));

    for (i=0; i<nrows; i++)
    G_get_map_row( thinriver_fd, nimg+i*ncols, i);

    G_close_cell(thinriver_fd);

    max_fd = G_fopen_old("watershed/max", river_name, river_mapset);
    if (!max_fd)
	exit(2);

    if (fscanf(max_fd, "%d %d", &startr, &startc) != 2)
    {
	fprintf(stderr,"error obtaining outlet point\n");
	exit(2);
    }

    fclose(max_fd);

    if (nimg[startr*ncols + startc] != 1)
    {
	fprintf(stderr,"error:  outlet point not included in stream network\n");
	exit(3);
    }

    ptr_fd = G_open_cell_old(ptr_name, ptr_mapset);
    if (ptr_fd < 0)
        exit(2);

    ptr_row = G_allocate_cell_buf();

    G_get_map_row( ptr_fd, ptr_row, startr);

    ptr_val = (int)*(ptr_row + startc);
/*fprintf(stderr,"ptr is %d\n",ptr_val); */

    G_close_cell(ptr_fd);
    free(ptr_row);

/*
    ptr_val -= 4;
    if (ptr_val < 0)
	ptr_val += 8;
*/

    for (r=0; r<3; r++)
	for (c=0; c<3; c++)
	    if (ptr_val == expo[r][c])
	    {
		startrpt = startr + (r-1);
		startcpt = startc + (c-1);
	    }
/*
fprintf(stderr,"starting pointers are %d (row) and %d (col)\n",
  startrpt, startcpt);
*/

    
    for(r=0;r<nrows;r++)
        for(c=0;c<ncols;c++)
            if (nimg[r*ncols+c]==1) nimg[r*ncols+c]=10000;

/* set starting and pointer pixel coordinates */
    nodecnt=1;
    r=startr;
    c=startc;
    ro=startrpt;
    co=startcpt;
    startflag = 1;

/* temporary fix of network */
    nimg[ro*ncols+co]=1;
    fprintf(node_fd,"%d %d %d ",nodecnt,r,c);
/*    printf("%d %d %d \n",nodecnt,r,c); */

printf("calling climber...\n");
/* call climber subroutine to recursively mark and code network */
    climber(r,c,ro,co);
    printf("back from climber\n");

/* undo temporary fix of network */
    nimg[startrpt*ncols+startcpt]=0;

    fclose (node_fd);
    fclose (renum_fd);
    fclose (drain_fd);

    nodecnt--;
    node = (topo *)G_calloc(nodecnt,sizeof(topo));

    node_fd = G_fopen_old("watershed/nodes",thin_name,G_mapset());
    if (!node_fd)
    {
        fprintf(stderr,"cannot reopen nodes file\n");
        exit(3);
    }

    for (i=0;i<nodecnt;i++)
    {
        if (temp = fscanf(node_fd,"%d %d %d %d %d %d %d %lf\n",
            &node[i].num,&node[i].r1,&node[i].c1,&node[i].num,
            &node[i].rend,&node[i].cend,&node[i].code,
            &node[i].chlength) != 8)
        {
            fprintf(stderr,"error in node storing file \n");
            exit(7);
        }
    }

    fclose(node_fd);

    renum = (int *)G_calloc(nodecnt+1,sizeof(int));

    renum_fd = G_fopen_old("watershed/renum",thin_name,G_mapset());
    if (!renum_fd)
    {
        fprintf(stderr,"cannot reopen renum file\n");
        exit(3);
    }

    for (i=0;i<nodecnt;i++)
    {
        if (temp = fscanf(renum_fd,"%d %d\n", &newnum, &oldnum) != 2)
        {
            fprintf(stderr,"error in renum file \n");
            exit(7);
        }
        renum[oldnum] = newnum;
    }

    fclose(renum_fd);

/*
    for (i=0; i<=nodecnt; i++)
    {
        fprintf(stderr,"renumbering %d to %d\n", i, renum[i]);
    }
*/

    drain = (drainage *)G_calloc(nodecnt-1,sizeof(drainage));

    drain_fd = G_fopen_old("watershed/drain",thin_name,G_mapset());
    if (!drain_fd)
    {
        fprintf(stderr,"cannot reopen drain file\n");
        exit(3);
    }

    for (i=0;i<nodecnt-1;i++)
    {
        if (temp = fscanf(drain_fd,"%d drains into %d\n",
            &drain[i].numfrom,&drain[i].numinto) != 2)
        {
            fprintf(stderr,"error in drain file \n");
            exit(7);
        }
    }

    fclose(drain_fd);

    for (i=0; i<nodecnt-1; i++)
    {
        drain[i].numfrom = renum[drain[i].numfrom];
        drain[i].numinto = renum[drain[i].numinto];
        node[i].num = renum[node[i].num];
    }
    node[nodecnt-1].num = renum[node[nodecnt-1].num];

/* if product asked for, write out marked network matrix */

    if (link_flag)
    {
        for(r=0;r<nrows;r++)
        {
            for(c=0;c<ncols;c++)
            {
                if(nimg[r*ncols+c]==10000) nimg[r*ncols+c] = 1;
                nimg[r*ncols+c] = renum[nimg[r*ncols+c]];
            }
            G_put_map_row(link_fd, nimg+r*ncols, r);
        }
        G_close_cell (link_fd);
    }

    node_fd = G_fopen_new("watershed/nodes",thin_name);
    if (!node_fd)
    {
        fprintf(stderr,"error trying to rewrite node file\n");
        exit(7);
    }

    fprintf(node_fd,"%d\n",nodecnt);

    for (i=0; i<nodecnt; i++)
        fprintf(node_fd,"%d %d %d %d %d %d %d %lf\n",i+1,
          node[i].r1,node[i].c1,node[i].num,node[i].rend,
          node[i].cend,node[i].code,node[i].chlength);

    fclose(node_fd);

    drain_fd = G_fopen_new("watershed/drain",thin_name);
    if (!drain_fd)
    {
        fprintf(stderr,"error trying to rewrite drain file\n");
        exit(7);
    }

    for (i=0;i<nodecnt-1;i++)
        fprintf(drain_fd,"%d drains into %d\n",
            drain[i].numfrom,drain[i].numinto);

    fclose(drain_fd);

    if (link_flag)
    {
        ncats = G_number_of_cats(link_name,G_mapset());
	if (G_make_random_colors(&colors,0,ncats) == -1)
        {
            fprintf(stderr,"error in assigning color table to output map\n");
            exit(2);
        }
        G_write_colors(link_name,G_mapset(),&colors);
    }

    exit(0);
}


climber(r,c,ro,co)

    int r,c,ro,co;

{
    int cn,ipt,inc,r1,c1,mark,rdiff,cdiff,nbr,row,col,con;
    int savenode;
    static int kernel[3][3] =
    {
          {4,3,2},
          {5,0,1},
          {6,7,8}
    };

/* if pixel is on the border, consider as source node and return */
    if(r <= 0 || r >= nrows-1 || c <= 0 || c >= ncols-1)
    {
        mark=0;
        nimg[r*ncols+c]=nodecnt;
        if (nodecnt != 1) fprintf(renum_fd,"%d %d\n",++watshed_num,nodecnt);
        if (!startflag)
        {
            if ((abs(r-ro)==1) && (abs(c-co)==1))
                chlength = chlength + diag;
            else
            {
                if (abs(r-ro)==1) chlength = chlength + (double)window.ns_res;
                else chlength = chlength + (double)window.ew_res;
            }
        }
        fprintf(node_fd," %d %d %d 1 %f\n",nodecnt,r,c,chlength);
        ++nodecnt;
        if (nodecnt == 10000)
        {
            fprintf(stderr,"Error -- not able to process over 10000 channel segments\n");
            exit(8);
        }
        chlength = 0;
        startflag = 0;
        flag=1;
        return;
    }
    else flag=0;

    cn=connum(r,c);   /* returns connectivity number */
    if(cn!=2)
    {
        if(cn==1)
        {
            mark=0;
            fprintf(renum_fd,"%d %d\n",++watshed_num,nodecnt);
        }
        else if(cn>2)
        {
            mark=cn-1;
            savenode = nodecnt;
        }
        nimg[r*ncols+c]=nodecnt;
        if (!startflag)
        {
            if ((abs(r-ro)==1) && (abs(c-co)==1))
                chlength = chlength + diag;
            else
            {
                if (abs(r-ro)==1) chlength = chlength + window.ns_res;
                else chlength = chlength + window.ew_res;
            }
        }
        fprintf(node_fd," %d %d %d %d %f\n",nodecnt,r,c,mark,chlength);
        ++nodecnt;
        if (nodecnt == 10000)
        {
            fprintf(stderr,"Error -- not able to process over 10000 channel segments\n");
            exit(8);
        }
        chlength = 0;
        startflag = 0;
        flag=1;
    }
    else flag=0;

/* set nimg to node */   
    if (cn==2)
    {
        nimg[r*ncols+c]=nodecnt;
        if (!startflag)
           {
            if ((abs(r-ro)==1) && (abs(c-co)==1))
                chlength = chlength + diag;
            else
            {
                if (abs(r-ro)==1) chlength = chlength + window.ns_res;
                else chlength = chlength + window.ew_res;
            }
        }
        else
            startflag=0;
    }

/* set up neighborhood search for next pixel */
    rdiff=ro-r;
    cdiff=co-c;
    ipt=kernel[rdiff+1][cdiff+1];

/* start search at pixel immediately clockwise to last pixel */
    ipt -= 2;
    if(ipt<1) ipt += 8;

/* loop around in clockwise direction */
    for(inc=0;inc<=7;inc++)
    {
        nbr=ipt-inc;
        if(nbr<1) nbr=8+nbr;
        nabor(nbr,&r1,&c1,r,c);

/* check whether marked, segment or node */
        if (nimg[r1*ncols+c1]==10000)
        {
            con = connum(r1,c1);
            if (con == 2)
            {
                nbr = nbr - 1;
                if(nbr<1) nbr=8+nbr;
                nabor(nbr,&row,&col,r,c);
                if (nimg[row*ncols+col]  == 10000)
                {
                    r1 = row;
                    c1 = col;
                }
            }
            if(flag==1)
            {
                fprintf(node_fd,"%d %d %d ",nodecnt,r1,c1);
                startflag=1;
                fprintf(drain_fd,"%d drains into %d\n",nodecnt,savenode);
            }
            ++inc;
            climber(r1,c1,r,c);
        }
        else if(nimg[r1*ncols+c1]>0 && nimg[r1*ncols+c1]<10000)
        {
            if (cn>2)
                fprintf(renum_fd,"%d %d\n",++watshed_num,nimg[r*ncols+c]);
            return;
        }

/* reset values of r1 and c1 */
        r1=r;
        c1=c;
    }
}


nabor(nbr,r1,c1,r,c)

    int nbr,*r1,*c1,r,c;

{
    if(nbr==1)
    {
        *c1=c+1;
        *r1=r;
    }
    else if(nbr==2)
    {
        *r1=r-1;
        *c1=c+1;
    }
    else if(nbr==3)
    {
        *r1=r-1;
        *c1=c;
    }
    else if(nbr==4)
    {
        *r1=r-1;
        *c1=c-1;
    }
    else if(nbr==5)
    {
        *c1=c-1;
        *r1=r;
    }
    else if(nbr==6)
    {
        *r1=r+1;
        *c1=c-1;
    }
    else if(nbr==7)
    {
        *r1=r+1;
        *c1=c;
    }
    else if(nbr==8)
    {
        *r1=r+1;
        *c1=c+1;
    }
}


/* routine for calculating crossing number */

connum(r,c)

    int r,c;

{
    int i,j,b[9],k,conn;

    k=0;
    for(i= -1;i<=1;i++)
    {
        for(j= -1;j<=1;j++)
        {
            b[k]=(nimg[(r+i)*ncols+(c+j)]>0) ? 0 : 1;
            ++k;
        }
    }

    conn = (abs(b[5]-b[2]) + abs(b[2]-b[1]) + abs(b[1]-b[0])
             +abs(b[0]-b[3]) + abs(b[3]-b[6]) + abs(b[6]-b[7])
             + abs(b[7]-b[8]) + abs(b[8]-b[5]))/2;
    return conn;

}
