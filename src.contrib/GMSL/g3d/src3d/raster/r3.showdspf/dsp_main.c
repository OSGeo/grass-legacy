/* for use on the Silicon Graphics Workstations only */


#define TOGGLE(x) ((x) = (x) ? 0 : 1)

#define MAIN
/*
#define GRID
*/


/*
#define DEBUG1
*/

#include "vizual.h"
#include <device.h>
#include <math.h>
#include <sys/types.h>
#include <ctype.h>

char *rindex();
Cube_data        CUBE;   /* and the data for a single cube */
 
void do_draw();
void do__draw();
void do__draw_solid();
void clear_screen();

static void (*Redrawfunc)();
static void (*Clearfunc)();
static void (*Scalefunc)();

static int Loaded=0;
static char Last_cmd[300];
static int Scalefuncset=0, Redrawset=0, Clearset=0;
static char ctablefile[256];
static struct dspec D_spec;


dspf_load(argc,argv)
int argc;
char *argv[];
{
/* Note: D_Cap is a global to main only who passes it to other functions */


    if(Loaded) dspf_close();
    
    if(!Redrawset)
	Redrawfunc = clear_screen;

    if(!Clearset)
	Clearfunc = clear_screen;

    if(!Scalefuncset)
	Scalefunc = clear_screen;

    if(check_cmndln(argc,argv) < 1)
    {
	pr_cmndln(argc, argv);
	return(-1);
    }

    G_sign = -1;
    X_sign = 1;

    
    /* opens grid3 file to read in original data */
    
    if(-1 == init_caps (argv[1],&D_Cap,&G3header))
	return(-1);
    print_head_info (&G3header);
    fprintf(stderr, "Grid3 file loaded > %s\n\n", argv[1]);
     
    if((Headfax.dspfinfp = fopen (argv[2],"r")) == NULL)
    {
	fprintf(stderr,"ERROR: unable to open %s for reading\n", argv[2]); 
	return(-1);
    }
    
    /* read header info from dspf file into GLOBAL variable Headfax */
    if(dfread_header(&Headfax) < 0)
    {
	fprintf(stderr,"ERROR:  while reading dspf file header\n");
	return(-1);
    }
    copy_head (&G3header, &Headfax);

    fprintf(stderr, "Display file loaded > %s\n\n", argv[2]);

    /* going to check to see if there is any difference between these two*/
    /* the xyzdims should not match because one has gradient iso, the other
    raw data*/
    /*diff_header(&G3header,&Headfax);*/
    Headfax.datainfp = G3header.datainfp;
    new_dspf(&Headfax);
	
    /* INIT */
    strcpy (ctablefile, argv[3]);
    init_dspec (ctablefile);

    /* INIT OF THE D_spec.B or D_spec.E CAN HAPPEN MORE THAN ONCE */
    init_bounds();

    D_spec.c_flag = 1;	/* reset flag */
    Loaded = 1;

#ifdef OLD
    init_graphics(argv, &D_spec);
    winset (Window[1]);
    D_spec.Swap_buf = 1;
    Toggle_swapbuffers (&D_spec);	/* make sure they sync up */
    Toggle_swapbuffers (&D_spec);
#endif
    
    return (1);
}

dspf_set()
{
char	buf[300];
char	*p;
int         ret;

    if(Loaded){

	options();

	while (1)   /* looking for input from the keyboard */
	{
#ifdef OLD
	    int foo;
	    short val;

	    if (qtest ())
		switch (foo = qread (&val)) {
		    case REDRAW:
			Redrawfunc ();
			break;
		}
#endif	
	    /* get input from the keyboard */
	    fprintf(stderr,"enter desired manipulations then press return\n\n");
	    fprintf(stderr,"? t T# + - (xyz)# (XYZ)# r S B(xyz)# E(xyz)# R C c s w d Q h\n");
	    fprintf (stderr, " > ");
	    
	    if (NULL == gets (buf))
		break;	
	    p = buf;	

	    /* if returns > 1 (drawable), copy to last_cmd */
	    ret = dispatch_cmd(p);
	    if(ret > 1)
		strcpy(Last_cmd, buf);
	    else if(ret == 0)
		return(ret);
	}
    }
    return(-1);
}

dispatch_cmd(p)
char *p;
{
    int tmp, tmp2,drawable_cmd;
    char cmd, axis;
    static int dobox = DRAW_BBOX;
    char cfilename[128], ctablename[128];
    
    drawable_cmd = 0;
    while (*p) /* assign valid keyboard entries to D_spec structure */
    {
	cmd = *p++;
	switch (cmd)
	{
	    case '#':  /* rest of line is a comment */
		*p = 0;	/* throw away rest of line */
		break;
	    case 'n':
		G_sign = -G_sign;
		X_sign = -X_sign;
		break;
	    case 'b':
		dobox = dobox? 0: DRAW_BBOX;
		break;

	    case '+':
		D_spec.Thresh++;
		if (D_spec.Thresh > Headfax.linefax.nthres-1)
		    D_spec.Thresh = 0;
		do_draw (&Headfax, &G3header, &D_spec, &D_Cap, DRAW_ISO|dobox);
		drawable_cmd = 1;
		break;

	    case '-':
		D_spec.Thresh--;
		if (D_spec.Thresh < 0)
		    D_spec.Thresh = Headfax.linefax.nthres-1;
		do_draw (&Headfax, &G3header, &D_spec, &D_Cap, DRAW_ISO|dobox);
		drawable_cmd = 1;
		break;
	    case '?':
		{
		    int i;
		    for ( i=0; i<Headfax.linefax.nthres; i++)
			fprintf(stderr,"%c %3d for threshold value %5.2f\n",
			  i == D_spec.Thresh ? '*' : ' ', i+1, 
			  Headfax.linefax.tvalue[i]);
		    fprintf (stderr, "Rotations: X %d  Y %d  Z %d\n", D_spec.xrot, D_spec.yrot, D_spec.zrot);
		}
		break;

	    case 't':   /* show only this threshold */
		G_squeeze (p);
		if (isdigit(*p))
		{
		    int i;
		    i = atoi (p);
		    if (i < 1 || i > Headfax.linefax.nthres)
			fprintf (stderr, "Range is 1 to %d\n", Headfax.linefax.nthres);
		    D_spec.Thresh = i-1;
		    /*  ENTER CODE TO REDRAW */
		    *p = 0;	/* throw away rest of line */
		}
		else 
		    fprintf(stderr,"check keyboard entry instructions \n"), *p = 0;
		break;
	    case 'T':   /* show only this threshold */
		G_squeeze (p);
		if (2 != sscanf (p, "%d %d", &(D_spec.low), &(D_spec.hi)))
		{
/*DEBUG*/ 	
fprintf (stderr, ":><>>>  T %d %d\n", (D_spec.low), (D_spec.hi));
fprintf(stderr,"check keyboard entry instructions \n"); 
		    *p = 0;
		    D_spec.low = 0; D_spec.hi = Headfax.linefax.nthres - 1;
		}
		else
		{
		    D_spec.low--;	/* convert from user to internal #s */
		    D_spec.hi--;
		}
		*p = 0;	/* throw away rest of line */
		break;

	    case 'B': /* initial value along specified axis */ 
	    case 'E': /* ending  value along specified axis */ 
		    G_squeeze (p);
		    axis = *p++;
		    G_squeeze (p);
		    if (isdigit(*p))
		    {
			G_squeeze (p);
			tmp = atoi(p);
			*p = 0;/*throw away rest of line */
		    }
		    else
		    {
			fprintf(stderr,"enter number also\n");
			break;
		    }
		    switch (axis)
		    {
			case 'x':
			    if (cmd == 'B') 
				D_spec.B[X]  =  tmp;
			    else
				D_spec.E[X]  =  tmp;
			    tmp = X;
			    break;
			case 'y':
			    if (cmd == 'B') 
				D_spec.B[Y]  =  tmp;
			    else
				D_spec.E[Y]  =  tmp;
			    tmp = Y;
			    break;
			case 'z':
			    if (cmd == 'B') 
				D_spec.B[Z]  =  tmp;
			    else
				D_spec.E[Z]  =  tmp;
			    tmp = Z;
			    break;
		    }	
		    check_limits (&D_spec, tmp);
		    break;
	    case 'R':
		init_bounds(); 
		break;
	    case 'S':  /* Specular highlight */
		G_squeeze (p);
		if (isdigit(*p))
		{
		    D_spec.Specular = (float)atof(p);
		    *p = 0;
#ifdef OLD
		    change_spec (D_spec.Specular);
#endif
		}
		else 
		    fprintf(stderr,"check keyboard entry instructions \n"), *p = 0;
		break;
	    case 'r':
#ifdef OLD
		rotate_model ();
		/* in case mode has been changed */
		if (D_spec.Swap_buf)
		    doublebuffer ();
		else
		    singlebuffer ();
		gconfig ();
		Clearfunc();
#endif
		break;
	    case 's':  /* use swapbuffers */
		Toggle_swapbuffers (&D_spec);

		break;
	    case 'd':    /* draw it */
		G_squeeze (p);
		if (isdigit(*p))
		{
		    int i;
		    i = atoi (p);

		    if (i == 1)
			D_spec.Thresh = D_spec.low;
		    else
			D_spec.Thresh = D_spec.hi;
		}
		do_draw (&Headfax, &G3header, &D_spec, &D_Cap, DRAW_ISO|dobox);
		drawable_cmd = 1;
		*p = 0;
		break;
	    case 'u':    /* Update the screen in double buffer mode */
		swapbuffers ();
		break;
	    case 'D':    /* draw solid */
		do_draw (&Headfax, &G3header, &D_spec, &D_Cap, DRAW_SOLID|dobox);
		drawable_cmd = 1;
		break;
	    case 'x': /* rotate around x-axis */
		G_squeeze (p);
		if (isdigit(*p))
		{
		    D_spec.xrot = atoi(p);
		    *p = 0;
		}
		else 
		    fprintf(stderr,"check keyboard entry instructions \n"), *p = 0;
		break;
	    case 'X': /* auto rotate around x-axis */
		G_squeeze (p);
		if (isdigit(*p))
		{
		    D_spec.xscale = atof(p);
		    Scalefunc();
		    *p = 0;
		}
		else 
		    fprintf(stderr,"check keyboard entry instructions \n"), *p = 0;
		break;
	    case 'y': /* rotate around y-axis */
		G_squeeze (p);
		if (isdigit(*p))
		{
		    D_spec.yrot = atoi(p);
		    *p = 0;
		}
		else 
		    fprintf(stderr,"check keyboard entry instructions \n"), *p = 0;
		break;
	    case 'Y': /* auto rotate around y-axis */
		G_squeeze (p);
		if (isdigit(*p))
		{
		    D_spec.yscale = atof(p);
		    Scalefunc();
		    *p = 0;
		} 
		else 
		    fprintf(stderr,"check keyboard entry instructions \n"), *p = 0;
		break;
	    case 'z': /* rotate around z-axis */
		G_squeeze (p);
		if (isdigit(*p))
		{
		    D_spec.zrot = atoi(p);	
		    *p = 0;
		}
		else 
		    fprintf(stderr,"check keyboard entry instructions \n"), *p = 0;
		break;
	    case 'Z': /* autorotate around z-axis */
		G_squeeze (p);
		if (isdigit(*p))
		{
		    D_spec.zscale = atof(p);
		    Scalefunc();
		    *p = 0;
		}
		else 
		    fprintf(stderr,"check keyboard entry instructions \n"), *p = 0;
		break;
	    case 'C':  /* toggle clear flag */
		TOGGLE (D_spec.c_flag);
		break;
	    case 'c': /* redraw the screen */
		Clearfunc();
		break;
	    case 'w':   /* dump image to file */
		dumprect (G_squeeze (++p));
		*p = 0;
		break;
	    case 'W':   /* TEST dump image to file */
		dumprect2 (G_squeeze (++p));
		*p = 0;
		break;
	     /* TEST READ image from file */
	  /*case 'L':
		loadrect (G_squeeze (++p));
		*p = 0;
		break;
	*/

	    case 'l':   /* list thresholds */
		D_spec.nt = 0;
		G_squeeze (p);
		while  (isdigit(*p))
		{
		    int i;
		    i = atoi (p);
		    if (i < 1 || i > Headfax.linefax.nthres)
			fprintf (stderr, 
			    "Range is 1 to %d\n", Headfax.linefax.nthres);
		    else
		    {
		        D_spec.t[D_spec.nt] = i-1;
		        D_spec.nt++;

		    }
		    p++;
		    if (i >= 10)
		        p++;
		    if (i >= 100)
		        p++;
		    G_squeeze (p);
		}
		*p = 0;	/* throw away rest of line */
		break;
	    case 'L': /*display list*/
	        if (!D_spec.nt)
		    break;

		if (D_spec.c_flag)
		    Clearfunc();
                tmp = D_spec.c_flag;
		D_spec.c_flag = 0;
		tmp2 = D_spec.Thresh;
		for (j = 0; j < D_spec.nt; j++)
		{
		    D_spec.Thresh = D_spec.t[j];
		    do_draw 
		    (&Headfax, &G3header, &D_spec, &D_Cap, DRAW_ISO|dobox);
		}
                D_spec.c_flag = tmp;
		D_spec.Thresh = tmp2;
		drawable_cmd = 1;
		break;
	    case 'Q':	/* QUIT */
		return (0);
		break;
	    case 'h':	/* help */
		options();	
		break;
	    case 'p':	/* display a single plane 1-6 */
		G_squeeze (p);
		if (isdigit(*p))
		{
		    D_spec.plane = atoi(p) - 1;
		    if( D_spec.plane >= 0 && D_spec.plane <= 5)
		    {
			/* call plane drawing code */
			do_draw (&Headfax, &G3header, &D_spec, &D_Cap, DRAW_CAP|( (1<<D_spec.plane) <<16) );
			drawable_cmd = 1;
		    }
		    else
			fprintf(stderr,"must be number between 1-6\n"); 
		}
		else{ 
		    /* call code to draw all planes */
		    do_draw (&Headfax, &G3header, &D_spec, &D_Cap, DRAW_CAP|(0x3f<<16));
		    drawable_cmd = 1;
		}
		*p = 0;
		break;
	    case 'I':  /* toggle in_out flag */
		TOGGLE (D_spec.in_out);
		break;
            case 'F':       /* new color File */
                    G_squeeze (p);
                    if (2 != sscanf (p, "%s %s", cfilename, ctablename))
                        no_color_file (&D_spec, ctablefile);
                    else
		    {
                        if (0 > new_color_file (cfilename, ctablename, &D_spec))
                            no_color_file (&D_spec, ctablefile);
		    }
                    *p = 0;
                    break;
            default: 
                    *p = 0;
                    break;
		   
	}
    }
    return(drawable_cmd? 2: 1);
}

dspf_get_zscale(zscale)
float *zscale;
{
    *zscale = D_spec.zscale; 
}

/* note that currently using G3header */
dspf_get_res(xres, yres, zres)
float *xres, *yres, *zres;
{
    *xres = (G3header.east - G3header.west)/G3header.xdim;
    *yres = (G3header.north - G3header.south)/G3header.ydim;
    *zres = (G3header.top - G3header.bottom)/G3header.zdim;
}

/* note that currently using G3header */
dspf_getorigin(west, south, bottom)
float *west, *south, *bottom;
{
#ifdef DEBUG
static int first=1;
if(first<10){ /* DEBUG */
fprintf(stderr,"WEST = %f\nSOUTH = %f\nBOTTOM = %f\n", *west, *south, *bottom);
first++;
}
#endif

    if(Loaded){
	*west = G3header.west;
	*south = G3header.south;
	*bottom = G3header.bottom;
	return(1);
    }
    return(-1);
}

void
do_draw (Headp, G3p, D_spec, Cap, type)
    file_info *Headp;
    file_info *G3p;
    struct dspec *D_spec;
    struct Cap *Cap;
    unsigned int type;	/* see vizual.h for DRAW_* defines */
{
    static int first = 1;
    static double x, y, z;

/*    if (first)*/
    {
	x = Headfax.xdim*D_spec->xscale/2;
	y = Headfax.ydim*D_spec->yscale/2;
	z = Headfax.zdim*D_spec->zscale/2;
    }

    if(getdisplaymode()){
        frontbuffer (1);
        backbuffer (0);
    }
 
    pushmatrix ();

#ifdef OLD
        /*do_translate (Headp, D_spec); */
	translate (0.0,0.0,D_spec->ztrans);	/* move it away from eye */

	if (D_spec->c_flag)
	    Clearfunc();

	/* first  xyz, then yxz, now zyx */

	rotate (D_spec->yrot * 10, 'y');
	rotate (D_spec->zrot * 10, 'z');
	rotate (D_spec->xrot * 10, 'x');

	translate (-x,-y,-z);
#endif	

	if (type & DRAW_BBOX)
	    do__bbox (D_spec);

	if (type & DRAW_ISO)
	    do__draw (Headp, D_spec);
	else if (type & DRAW_SOLID)
	    do__draw_solid (Headp, G3p,D_spec, Cap);
	else if (type & DRAW_CAP)
	    draw_cap_side (D_spec, Headp, G3p, Cap, (type >> 16) & 0x3f);
	    /* bring over arguement to DRAW_CAP in high 16 bits to low 16 */
	    /*    and AND with   00111111  for sides 1-6 */


	/* todo, only draw visible sides, and dont use Zbuffer */
	if (type & DRAW_BBOX)
	    do__bbox (D_spec);

#ifdef OLD
	if (D_spec->Swap_buf)
	    swapbuffers();
#endif

    popmatrix ();
    
    if(getdisplaymode()){
        frontbuffer (1);
        backbuffer (0);
    }

}

do__bbox (D_spec)
    struct dspec *D_spec;
{
    static int 		first = 1;
    static float	x,y,z;
    static float	c[8][3];/*holds the corner points for bounding box */
    static float	gxl[100][3],gxh[100][3],gyl[100][3],gyh[100][3];
    int			gx,gy,gz;
    char 		label[20];

#ifdef OLD
    lmbind (MATERIAL, 0);
#endif
    
    lmcolor(LMC_COLOR); 

    /*
    **  do rotations here
    */
    /*if (first)*/
    {
	x = Headfax.xdim*D_spec->xscale;
	y = Headfax.ydim*D_spec->yscale;
	z = Headfax.zdim*D_spec->zscale;

	/* init corner array */
	c[1][0] = c[2][0] = c[5][0] = c[6][0] = x;
	c[0][0] = c[3][0] = c[4][0] = c[7][0] = 0;
	c[0][1] = c[1][1] = c[4][1] = c[5][1] = y;
	c[3][1] = c[2][1] = c[7][1] = c[6][1] = 0;
	c[0][2] = c[1][2] = c[3][2] = c[2][2] = z;
	c[4][2] = c[5][2] = c[7][2] = c[6][2] = 0;

	/* init grid array */
#ifdef GRID
	for(gy = 1; gy < Headfax.ydim;gy++)
	{
   	   gyl[gy][0] =  0; gyh[gy][0] =  x;
	   gyl[gy][1] = gy*D_spec.yscale; gyh[gy][1] = gy*D_spec.yscale;
	   /*gyl[gy][2] =  z; gyh[gy][2] =  z;*/
	   gyl[gy][2] =  0; gyh[gy][2] =  0;
	}
	
	for(gx = 1; gx < Headfax.xdim;gx++)
	{
	   gxl[gx][0] = gx*D_spec.xscale; gxh[gx][0] = gx*D_spec.xscale;
   	   gxl[gx][1] =  0; gxh[gx][1] =  y;
	   /*gxl[gx][2] =  z; gxh[gx][2] =  z;*/
	   gxl[gx][2] =  0; gxh[gx][2] =  0;
	}


    /*init_plane(c,p,sides); this is a much earlier idea JCM */
	/* build plane vertex info (3verts)from the above info  used for normals*/
	/* based on the planes as defined in cap_data.c */
	/* to have normals pointing the correct direction CCW ordering of vertices*/

	/*side 0  xy plane z = zdim*/
	D_spec->p[0][0][0]=c[0][0]; D_spec->p[0][0][1]=c[0][1]; D_spec->p[0][0][2]=c[0][2]; 
	D_spec->p[0][1][0]=c[3][0]; D_spec->p[0][1][1]=c[3][1]; D_spec->p[0][1][2]=c[3][2]; 
	D_spec->p[0][2][0]=c[1][0]; D_spec->p[0][2][1]=c[1][1]; D_spec->p[0][2][2]=c[1][2];

	/*side 1 xy plane  z = 0 */
  D_spec->p[1][0][0]=c[4][0]; D_spec->p[1][0][1]=c[4][1]; D_spec->p[1][0][2]=c[4][2];
  D_spec->p[1][1][0]=c[5][0]; D_spec->p[1][1][1]=c[5][1]; D_spec->p[1][1][2]=c[5][2];
  D_spec->p[1][2][0]=c[7][0]; D_spec->p[1][2][1]=c[7][1]; D_spec->p[1][2][2]=c[7][2];

  /*side 2 yz plane x=xdim */
  D_spec->p[2][0][0] = c[1][0];D_spec->p[2][0][1] = c[1][1];D_spec->p[2][0][2] = c[1][2];
  D_spec->p[2][1][0] = c[2][0];D_spec->p[2][1][1] = c[2][1];D_spec->p[2][1][2] = c[2][2];
  D_spec->p[2][2][0] = c[5][0];D_spec->p[2][2][1] = c[5][1];D_spec->p[2][2][2] = c[5][2];

  /*side 3 yz plane x=0 */
  D_spec->p[3][0][0] = c[6][0];D_spec->p[3][0][1] = c[6][1];D_spec->p[3][0][2] = c[6][2];
  D_spec->p[3][1][0] = c[7][0];D_spec->p[3][1][1] = c[7][1];D_spec->p[3][1][2] = c[7][2];
  D_spec->p[3][2][0] = c[3][0];D_spec->p[3][2][1] = c[3][1];D_spec->p[3][2][2] = c[3][2];

  /*side 4 zx plane y=ydim*/
  D_spec->p[4][0][0] = c[4][0];D_spec->p[4][0][1] = c[4][1];D_spec->p[4][0][2] = c[4][2];
  D_spec->p[4][1][0] = c[0][0];D_spec->p[4][1][1] = c[0][1];D_spec->p[4][1][2] = c[0][2];
  D_spec->p[4][2][0] = c[1][0];D_spec->p[4][2][1] = c[1][1];D_spec->p[4][2][2] = c[1][2];

  /*side 5 zx plane y=0*/
  D_spec->p[5][0][0] = c[7][0];D_spec->p[5][0][1] = c[7][1];D_spec->p[5][0][2] = c[7][2];
  D_spec->p[5][1][0] = c[6][0];D_spec->p[5][1][1] = c[6][1];D_spec->p[5][1][2] = c[6][2];
  D_spec->p[5][2][0] = c[3][0];D_spec->p[5][2][1] = c[3][1];D_spec->p[5][2][2] = c[3][2];
#endif
  first = 0;
  }
    
    /* draw bounding box */
    cpack(0x0000ff);
    bgnclosedline();v3f(c[0]);v3f(c[1]);v3f(c[2]);v3f(c[3]);endclosedline();
    bgnline();v3f(c[0]);v3f(c[4]);endline();
    bgnline();v3f(c[1]);v3f(c[5]);endline();
    bgnline();v3f(c[2]);v3f(c[6]);endline();
    bgnline();v3f(c[3]);v3f(c[7]);endline();
    bgnclosedline();v3f(c[4]);v3f(c[5]);v3f(c[6]);v3f(c[7]);endclosedline();
    cmov(-0.5,y,z);charstr("0");
    cmov(x + 0.5,y,z);charstr("1");
    cmov(-0.5,0,z);charstr("3");
    cmov(x + 0.5,0,z);charstr("2");
    cmov(-0.5,y,0);charstr("4");
    cmov(x + 0.5,y,0);charstr("5");
    cmov(-0.5,0,0);charstr("7");
    cmov(x + 0.5,0,0);charstr("6");

#ifdef GRID
    for(gy = 1; gy < Headfax.ydim;gy++)
    {
	/*cmov(-0.5,gy*D_spec.yscale,z);*/
	cmov(-0.5,gy*D_spec.yscale,0);
	sprintf(label,"%d",gy);charstr(label);
	bgnline();v3f(gyl[gy]);v3f(gyh[gy]);endline();
	/*cmov(x+0.5,gy*D_spec.yscale,z);*/
	cmov(x+0.5,gy*D_spec.yscale,0);
	charstr(label);
    }

    for(gx = 1; gx < Headfax.xdim;gx++)
    {
	/*cmov(gx*D_spec.xscale,-0.5,z);*/
	cmov(gx*D_spec.xscale,-0.5,0);
	sprintf(label,"%d",gx);charstr(label);
	bgnline();v3f(gxl[gx]);v3f(gxh[gx]);endline();
	/*cmov(gx*D_spec.xscale,y+0.5,z);*/
	cmov(gx*D_spec.xscale,y+0.5,0);
	charstr(label);
    }
#endif
#ifdef OLD
    lmbind (MATERIAL, 1);
#endif

}

void
do__draw (Headp, D_spec)
    file_info *Headp;
    struct dspec *D_spec;
{
    static int first = 1;

    if (first)
	first = 0;
    else
	reset_reads (&Headfax);

    fprintf (stderr, "Threshold %d = %f\n",
        D_spec->Thresh + 1, Headp->linefax.tvalue[D_spec->Thresh]);


    switch (Headp->linefax.litmodel)
    {
	case 1:
	    fdraw_polys(&CUBE, D_spec);
	    break;
	case 2: case3:
	    gdraw_polys(&CUBE, D_spec);
	    break;
    }
}

void
do__draw_solid (Headp, G3header, D_spec, Cap)
    file_info *Headp;
    file_info *G3header;
    struct dspec *D_spec;
    struct Cap *Cap;
{
    int min, max;

    min = D_spec->low;
    max = D_spec->hi;


    D_spec->Thresh = min; do__draw (Headp, D_spec); /* Draw low thresh */
    D_spec->Thresh = max; do__draw (Headp, D_spec); /* Draw hi  thresh */

    /* TODO optimize  only change when lo/hi or in_out is changed */
    build_thresh_arrays (D_spec, Headp);
    draw_cap_side (D_spec, Headp, G3header, Cap, -1);
}


/************************  check_cmndln  *************************************/
/************************  check_cmndln  *************************************/
/************************  check_cmndln  *************************************/

check_cmndln(argc,argv)
int argc;
char *argv[];
{
    char buf[200], *p = NULL;

    if (argc  != 3 && argc != 4)	/* dpg*/
	return(-1);

    /* compares the suffix of argv[1] to see if data file is grid3 file */
    strcpy(buf,argv[1]);

    p  = rindex(buf,'.');/*sets pointer to last occurrence of '.' */
    if(p == NULL)
        return(-1);
    
    strcpy(buf,p);    /*truncates buf to only include the suffix*/

    if(strcmp(buf,".grid3") != 0)
	return(-1);
    
    /* compares the suffix of argv[2] to see if data file is dspf file */
    strcpy(buf,argv[2]);

    p  = rindex(buf,'.');/*sets pointer to last occurrence of '.' */
    if(p == NULL)
        return(-1);
    
    strcpy(buf,p);    /*truncates buf to only include the suffix*/

    if(strcmp(buf,".dspf") != 0)
	return(-1);
    
    return(1);
}

/************************* pr_cmndln  *****************************************/
/************************* pr_cmndln  *****************************************/
/************************* pr_cmndln  *****************************************/

pr_cmndln(argc, argv)
    int argc;
    char **argv;
{
/*DEBUG*/fprintf (stderr, "\nGot: %s %s %s %s\n", argv[0],argv[1],argv[2],argv[3]);
    fprintf (stderr, "\nUsage: %s input.grid3 input.dspf input.clr\n\n", argv[0]);
}

/*************************** init_bounds *************************************/
/* this subroutine resets the display boundaries along the xyz axis */
init_bounds()
{
    D_spec.B[X] = 0;
    D_spec.B[Y] = 0;
    D_spec.B[Z] = 0;
    D_spec.E[X] = Headfax.xdim;	/* boundaries are outsides of cubelets */
    D_spec.E[Y] = Headfax.ydim;
    D_spec.E[Z] = Headfax.zdim;
}

init_dspec (ctable)
    char *ctable;
{
    D_spec.Thresh = 0;
    D_spec.nt = 0 ; 	   /* number of indexes chosen (cumulative)*/
    D_spec.xscale = 1.0;
    D_spec.yscale = 1.0;
    D_spec.zscale = 1.0;
    D_spec.xrot = 0;
    D_spec.yrot = 0;
    D_spec.zrot = 0; /* angle in degrees */ 
    D_spec.Xrot = 0;
    D_spec.Yrot = 0;
    D_spec.Zrot = 0; /* indicates if do autorotate */
    D_spec.ztrans = 0;
    D_spec.Specular = 10;
    D_spec.low = 0; 
    D_spec.hi = Headfax.linefax.nthres - 1;
    D_spec.in_out = 0; /*defined as INSIDE */
    if ( 0 > get_color_table (ctable, D_spec.ctable))
    {
        fprintf (stderr, "Using default color table\n");
        get_default_table (&Headfax, D_spec.ctable);
    }

}
/******************************* options ************************************/
options()
{
    /*DISPLAY INSTRUCTIONS FOR THE KEYBOARD INTERACTIVE PROGRAM */
    fprintf(stderr,"\nTHE INTERACTIVE OPTIONS ARE:\n\n");
    fprintf(stderr,"?, (t #), (T #), +, -\n");
    fprintf(stderr,"(x #) (y #) (z #) (X #) (Y #) (Z #)\n ");
    fprintf(stderr,"B(x,y,z)#), (E(x,y,z)#), R, d ,s ,w,c,Q\n");
    fprintf(stderr,"\nUSAGE AND MEANING:\n\n");
    fprintf(stderr,"?         lists available thresholds\n");
    fprintf(stderr,"t index#  add threshold to display list \n");
    fprintf(stderr,"T index#  reset so only this threshold is displayed\n");
    fprintf(stderr,"+(+++)    display thresholds with consecutively increasing index#\n");
    fprintf(stderr,"-(---)    display thresholds with consecutively decreasing index#\n\n");

#ifdef OLD
    fprintf(stderr,"x int#    absolute rotation around x-axis in degrees(int) \n");
    fprintf(stderr,"y int#    absolute rotation around y-axis in degrees(int) \n");
    fprintf(stderr,"z int#    absolute rotation around z-axis in degrees(int) \n");
    fprintf(stderr,"r  rotate_model\n");
    fprintf(stderr,"X int#   scale model in x/step\n");
    fprintf(stderr,"Y int#   scale model in y/step)\n");
    fprintf(stderr,"Z int#   scale model in z)\n\n");
    fprintf(stderr,"S int#    specular highlight control\n");
#endif
    fprintf(stderr,"B(x,y,z)int#  begin display along (x,y,z) axis at #\n"); 
    fprintf(stderr,"E(x,y,z)int#  end display along (x,y,z)axis #\n");
#ifdef OLD
    fprintf(stderr,"R   resets display along axis to show all data\n\n");
    fprintf(stderr,"C   toggles the c_flag\n");
#endif
    fprintf(stderr,"c   clears the display (no thresholds)\n");
#ifdef OLD
    fprintf(stderr,"s   swap buffers\n\n");
    fprintf(stderr,"w   dump image to a file\n"); 
#endif
    fprintf(stderr,"d   draw (implement the option)\n");
    fprintf(stderr,"Q   QUIT showdspf (return to SG3d)\n"); 
    fprintf(stderr,"h   help\n"); 
}

Toggle_swapbuffers (D_spec)
    struct dspec *D_spec;
{
#ifdef OLD
    TOGGLE (D_spec->Swap_buf);
    if (D_spec->Swap_buf)
	doublebuffer ();
    else
	singlebuffer ();
    gconfig ();
    /* and clear while we're here */
    Clearfunc();
#endif

}

void
clear_screen ()
{
    static short bg[] = {225, 225, 225};

    lmbind (MATERIAL, 0);
    c3s (bg);
    clear();
    zclear();
    lmbind (MATERIAL, 1);

}
/********************************************************************/
/* check begin & end planes for specified axis to make sure:
   1.) Begin < End
   2.) Begin > 0 & End < max */
/********************************************************************/
check_limits(D_spec, axis)
    struct dspec *D_spec;
    int axis;
{
    int max;
    int tmp;

    max = (axis == X) ? Headfax.xdim :
	  (axis == Y) ? Headfax.ydim : Headfax.zdim;

    if (D_spec->B[axis] > D_spec->E[axis])
    {
	tmp = D_spec->B[axis];
	D_spec->B[axis] = D_spec->E[axis];
	D_spec->E[axis] = tmp;
    }

    if (D_spec->B[axis] < 0)
        D_spec->B[axis] = 0;
    if (D_spec->E[axis] > max)
        D_spec->E[axis] = max;
}

dspf_close()
{
    if(Loaded){
	fclose(G3header.datainfp);
	fclose(Headfax.dspfinfp);
	if (D_spec.cfile)
	    fclose (D_spec.cfile);
	D_spec.cfile = G3header.datainfp = Headfax.dspfinfp = NULL;
	free(D_Cap.D_buff);
	D_Cap.D_buff = NULL;
	Loaded = 0;
    }
}

dspf_set_ZNexag(exag)
float exag;
{
    set_ZNexag(exag);
}

dspf_set_clear(newfunc)
void (*newfunc)();
{
    Clearfunc = newfunc;
    Clearset = 1;
}

dspf_set_redraw(newfunc)
void (*newfunc)();
{
    Redrawfunc = newfunc;
    Redrawset = 1;
}

dspf_set_scalefunc(newfunc)
void (*newfunc)();
{
    Scalefunc = newfunc;
    Scalefuncset = 1;
}

dspf_do_last_draw()
{
    if(Loaded)
	dispatch_cmd(Last_cmd);
}

extern int
dspy_bbox()
{
    do__bbox (&D_spec);
    return(0);
}

do_translate (Headp, D_spec)
    file_info *Headp;
    struct dspec *D_spec;
{
    float xd, yd, zd, trd;

    xd = Headp->xdim * D_spec->xscale;
    yd = Headp->ydim * D_spec->yscale;
    zd = Headp->zdim * D_spec->zscale;

    /* pick greatest dimension to use for translation of viewer from origin*/
    if(xd  < yd)
        trd  = yd;
    else
        trd = xd;
    if(trd < zd)
        trd = zd;
   
    translate (0.0,0.0,-trd*1.6);       /* move it away from eye */
}
copy_head (g3head, head)
    file_info *head, *g3head;
{
    head->north = g3head->north;
    head->south = g3head->south;
    head->east = g3head->east;
    head->west = g3head->west;
    head->top = g3head->top;
    head->bottom = g3head->bottom;
    head->ns_res = g3head->ns_res;
    head->ew_res = g3head->ew_res;
    head->tb_res = g3head->tb_res;
    head->zone = g3head->zone;
    head->proj = g3head->proj;
}

