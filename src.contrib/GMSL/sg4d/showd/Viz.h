
#include <stdio.h>
#include <math.h>
#include <sys/types.h>
#include <gl.h>


#define WITHIN(a,x,b)   (((a) <= (x) && (x) <= (b)) ? 1 : 0)

#define INSIDE  0
#define OUTSIDE 1

#define MAXTHRESH	127
#define MAXPOLY		 10

#define X 0
#define Y 1 
#define Z 2
#define DRAW_BBOX  1
#define DRAW_ISO   2
#define DRAW_SOLID 4
#define DRAW_CAP  8      

typedef struct 
{
    int		nthres; /* number of thresholds */
    float	tvalue[MAXTHRESH]; /* array of threshold values */
    int		litmodel; /* 1 = flat, 2 = gradient(6), 3 = gradient(26) */
}cmndln_info;

typedef struct
{
    int	token; /*type of file*/
    FILE	*datainfp,*dataoutfp,*dspfinfp,*dspfoutfp;
    int		xdim,ydim,zdim;
    float       north, south, east, west;
    float       top, bottom;
    float       ns_res, ew_res, tb_res;
    int         zone;
    int         proj;
    int		type; /*1 = short int, 2 = integer, 3 = float */
    float 	min,max; /* max and min values present in the data */
    long	Dataoff; /* offset of beginning of data in file */
    long	Lookoff; /* offset of beginning of lookup table in file */
    cmndln_info linefax; /* more global info */
    int headsize; /* size of datainf file header, necessary for random
                                               access to grid3 data */
}file_info;


typedef struct
{
    float	v1[3]; /*polygon vertices */
    float	v2[3];
    float	v3[3];
    float	n1[3],n2[3],n3[3];/*normals for vertices*/
}poly_info;

typedef struct
{
    int		npoly; /* number of polygons in cube at given threshold */
    int		t_ndx; /* index of given threshold */
    poly_info	poly[MAXPOLY];/*vertices and normals */
}cube_info;

typedef struct
{
    int		n_thresh;
    cube_info   data[MAXTHRESH];
}Cube_data;

struct poly_info {
    int vnum;/* the number of vertices in this polygon (= number of sides )*/
    double data[18];
    double verts[18];
};

struct color_entry {
    float data;
    short color[3];
};


struct dspec {
    int Thresh;
    int	t[MAXTHRESH]; 	/* an array of index numbers */
    int nt; 	   /* number of indexes chosen (cumulative)*/
    int xrot,yrot,zrot; /* angle in degrees */ 
    int Xrot,Yrot,Zrot; /* indicates if do autorotate */
    float xscale, yscale, zscale; /* scaling factor for each dimension */ 
    float ztrans;
    int B[3]; /* sets the minimum dim to be displayed along this axis */
		    /* the default is 0 */
    int E[3]; /* sets the maximum dim to be displayed along this axis */
		    /* the default is xdim, ydim or zdim */
    float	Xtran, Ytran, Ztran;   /* translation of object */
    int	c_flag;	/* reset flag */
    int Swap_buf;

    int low, hi;    /* outside threshold indexes */
    int in_out;    /* fill contours between thresholds or outside*/
			/* INSIDE 0  OUTSIDE 1 */
    /* Light model options */
    float Specular;
    int plane; /* which plane we are looking at */
	float p[6][3][3]; /*using the bounding box vertices for plane normals */
    cmndln_info threshes[2];
    struct color_entry ctable[101];
    FILE *cfile;
    int grid;
    int sign;
};


/*
**  Structure to support drawing end caps
*/
struct Cap {
    float *D_buff;		/* 2 dim data buffer */
    int reverse;		/* is it a mirror image? if so, polygons
				**  should be drawn counter-clockwise */
				/* 1 means do reverse */
    int minx,miny; /* sets the minimum dim to be displayed along this axis */
                    /* the default is 0 */
    int maxx,maxy; /* sets the maximum dim to be displayed along this axis */
                    /* the default is Rows, Cols  */
    int z; /* this is the axis that is a constant*/       		
    int side; /* which side 0 - 5 */
    int Cols, Rows;	/* Dims of current data in buffer */
    long offset;/*offset to data in grid3 file*/
};


