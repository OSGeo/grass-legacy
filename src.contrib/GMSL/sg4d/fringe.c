
/*
**  Written by Dave Gerdes  Summer 1990
**  US Army Construction Engineering Research Lab
*/

/*
** Copyright USA CERL 1992. All rights reserved.
*/

/*
#include "gis.h"
*/
#include "externs.h"

#define FRINGE_FORE 0xaaaaaa
#define FRINGE_BACK 0x660000
#define FRINGE_WIDTH 2

extern int viewcell_interp();
extern int edge_interp();

float Nnorth[] = { 0.0, 0.8, 0.6 };
float Nsouth[] = { 0.0, -0.8, 0.6 };
float Neast[] = { 0.8, 0.0, 0.6 };
float Nwest[] = { -0.8, 0.0, 0.6 };
float Ntop[] = { 0.0, 0.0, 1.0 };
float Nbottom[] = { 0.0, 0.0, -1.0 };


display_fringe (xmod, ymod)
    int xmod, ymod;  /* number of real cells per view cell */
{
    int row, col;
    float bot, xres, yres;   /* world size of view cell */
    int cnt;
    int ycnt, xcnt;    /* number of view cells across */
    
    xcnt = (X_Size % xmod? X_Size / xmod +1 : X_Size / xmod);
    ycnt = (Y_Size % ymod? Y_Size / ymod +1 : Y_Size / ymod);
    xres = X_Res * xmod;
    yres = Y_Res * ymod;

    /*bot = Z_Min_real - Z_Span_real/3;  TEST */
    if(Anozero->val)
	bot = (Z_Min_notzero - Zoff - Z_Span_real/4.) * Z_exag;
    else
	bot = Z_Min - Z_exag * Z_Span_real/4;

    linewidth (FRINGE_WIDTH);
    lmcolor(LMC_COLOR);        /* won't even use normals - no lighting */
    
    /* 
    **  for each side, update screen, then update Zbuffer
    */

    /* North fringe */
    n3f(Nnorth);
    cpack (FRINGE_BACK);
    zwritemask (0);
    fringe_horiz_poly (bot, xmod, ymod, xres, yres, xcnt, 0, 0);
    cpack (FRINGE_FORE); /* WHITE */
    fringe_horiz_line (bot, xmod, ymod, xres, yres, xcnt, 0, 0);
    zwritemask (0xffffffff);
    wmpack (0);
    fringe_horiz_poly (bot, xmod, ymod, xres, yres, xcnt, 0, 0);
    wmpack (0xffffffff);


    /* South fringe */
    n3f(Nsouth);
    cpack (FRINGE_BACK);
    zwritemask (0);
    fringe_horiz_poly (bot, xmod, ymod, xres, yres, xcnt, ycnt-2, 1);
    cpack (FRINGE_FORE); /* WHITE */
    fringe_horiz_line (bot, xmod, ymod, xres, yres, xcnt, ycnt-2, 1);
    zwritemask (0xffffffff);
    wmpack (0);
    fringe_horiz_poly (bot, xmod, ymod, xres, yres, xcnt, ycnt-2, 1);
    wmpack (0xffffffff);

    /* West fringe */
    n3f(Nwest);
    cpack (FRINGE_BACK);
    zwritemask (0);
    fringe_vert_poly (bot, xmod, ymod, xres, yres, ycnt, 0, 0);
    cpack (FRINGE_FORE); /* WHITE */
    fringe_vert_line (bot, xmod, ymod, xres, yres, ycnt, 0, 0);
    zwritemask (0xffffffff);
    wmpack (0);
    fringe_vert_poly (bot, xmod, ymod, xres, yres, ycnt, 0, 0);
    wmpack (0xffffffff);

    /* East fringe */
    n3f(Neast);
    cpack (FRINGE_BACK);
    zwritemask (0);
    fringe_vert_poly (bot, xmod, ymod, xres, yres, ycnt, xcnt-2, 1);
    cpack (FRINGE_FORE); /* WHITE */
    fringe_vert_line (bot, xmod, ymod, xres, yres, ycnt, xcnt-2, 1);
    zwritemask (0xffffffff);
    wmpack (0);
    fringe_vert_poly (bot, xmod, ymod, xres, yres, ycnt, xcnt-2, 1);
    wmpack (0xffffffff);

    linewidth (1);

}

fringe_horiz_poly (bot, xmod, ymod, xres, yres, xcnt, row, side)
    int xmod, ymod;  /* number of real cells per view cell */
    float bot, xres, yres;   /* world size of view cell */
    int row, xcnt;    /* number of view cells across */
    int side;
{
    int col;
    int cnt;

    bgnpolygon ();

    col = 0;
    /* floor left */
    vert_func (X_Min + col*xres, Y_Max - (row+side)*yres, bot);
    /* bottom left */ 
    vert_func (X_Min + col*xres, Y_Max - (row+side)*yres, 
	(float)((elev_buf[((int)((row+side)*ymod))*X_Size+(int)(col*xmod)] - Zoff) * Z_exag));
	    
    cnt = 1;
    for (col = 0 ; col < xcnt-1 ; col++)
    {
	/* bottom right */
	vert_func (X_Min + (col+1)*xres, Y_Max - (row+side)*yres, 
	    (float)((elev_buf[((int)((row+side)*ymod))*X_Size+(int)((col+1)*xmod)] - Zoff) * Z_exag));
	cnt++;

	if (cnt > 251)
	{
	    vert_func (X_Min + (col+1)*xres, Y_Max - (row+side)*yres, bot);
	    endpolygon ();
	    bgnpolygon ();
	    vert_func (X_Min + (col+1)*xres, Y_Max - (row+side)*yres, bot);
	    cnt = 0;
	    col--;  /* back up one */
	}
    }
    col--;
    vert_func (X_Min + (col+1)*xres, Y_Max - (row+side)*yres, bot);
    endpolygon ();
}

fringe_horiz_line (bot, xmod, ymod, xres, yres, xcnt, row, side)
    int xmod, ymod;  /* number of real cells per view cell */
    float bot, xres, yres;   /* world size of view cell */
    int row, xcnt;    /* number of view cells across */
    int side;
{
    int col;
    int cnt;

    bgnline ();

    col = 0;
    /* floor left */
    vert_func (X_Min + col*xres, Y_Max - (row+side)*yres, bot);
    /* bottom left */ 
    vert_func (X_Min + col*xres, Y_Max - (row+side)*yres, 
	(float)((elev_buf[((int)((row+side)*ymod))*X_Size+(int)(col*xmod)] - Zoff) * Z_exag));
	    
    cnt = 1;
    for (col = 0 ; col < xcnt-1 ; col++)
    {
	/* bottom right */
	vert_func (X_Min + (col+1)*xres, Y_Max - (row+side)*yres, 
	    (float)((elev_buf[((int)((row+side)*ymod))*X_Size+(int)((col+1)*xmod)] - Zoff) * Z_exag));
	cnt++;

	if (cnt > 251)
	{
	    /* floor middle */
	    endline ();
	    bgnline ();
	    cnt = 0;
	    col--;  /* back up one */
	}
    }
    col--;
    vert_func (X_Min + (col+1)*xres, Y_Max - (row+side)*yres, bot);
    vert_func (X_Min + 0*xres, Y_Max - (row+side)*yres, bot);
    endline ();
}

fringe_vert_poly (bot, xmod, ymod, xres, yres, ycnt, col, side)
    int xmod, ymod;  /* number of real cells per view cell */
    float bot, xres, yres;   /* world size of view cell */
    int col, ycnt;    /* number of view cells across */
{
    int row;
    int cnt;

    bgnpolygon ();

    row = 0;
    /* floor left */
    vert_func (X_Min + (col+side)*xres, Y_Max - row*yres, bot);
    /* bottom left */ 
    vert_func (X_Min + (col+side)*xres, Y_Max - row*yres, 
	(float)((elev_buf[((int)(row*ymod))*X_Size+(int)((col+side)*xmod)] - Zoff) * Z_exag));
	    
    cnt = 1;
    for (row = 0 ; row < ycnt-1 ; row++)
    {
	/* top left */
	vert_func (X_Min + (col+side)*xres, Y_Max - (row+1)*yres,
	    (float)((elev_buf[((int)((row+1)*ymod))*X_Size+(int)((col+side)*xmod)] - Zoff) * Z_exag));
	cnt++;

	if (cnt > 251)
	{
	    /* floor middle */
	    vert_func (X_Min + (col+side)*xres, Y_Max - (row+1)*yres, bot);
	    endpolygon ();
	    bgnpolygon ();
	    cnt = 1;
	    vert_func (X_Min + (col+side)*xres, Y_Max - (row+1)*yres, bot);
	    row--;  /* back up one */
	}
    }
    row--;
    vert_func (X_Min + (col+side)*xres, Y_Max - (row+1)*yres, bot);
    endpolygon ();
}

fringe_vert_line (bot, xmod, ymod, xres, yres, ycnt, col, side)
    int xmod, ymod;  /* number of real cells per view cell */
    float bot, xres, yres;   /* world size of view cell */
    int col, ycnt;    /* number of view cells across */
{
    int row;
    int cnt;

    bgnline ();

    row = 0;
    /* floor left */
    vert_func (X_Min + (col+side)*xres, Y_Max - row*yres, bot);
    /* bottom left */ 
    vert_func (X_Min + (col+side)*xres, Y_Max - row*yres, 
	(float)((elev_buf[((int)(row*ymod))*X_Size+(int)((col+side)*xmod)] - Zoff) * Z_exag));
	    
    cnt = 1;
    for (row = 0 ; row < ycnt-1 ; row++)
    {
	/* top left */
	vert_func (X_Min + (col+side)*xres, Y_Max - (row+1)*yres,
	    (float)((elev_buf[((int)((row+1)*ymod))*X_Size+(int)((col+side)*xmod)] - Zoff) * Z_exag));
	cnt++;

	if (cnt > 251)
	{
	    /* floor middle */
	    endline ();
	    bgnline ();
	    cnt = 0;
	    row--;  /* back up one */
	}
    }
    row--;
    vert_func (X_Min + (col+side)*xres, Y_Max - (row+1)*yres, bot);
    vert_func (X_Min + (col+side)*xres, Y_Max - 0*yres, bot);
    endline ();
}

/* displays a line drawn box in xy plane
at elevation of highest corner (or center if higher) of box */

float
display_box(center, siz, co, linew, off)
float center[3], siz;
long co;
int linew, off;
{
float max, corn[4][3];
double tmp[3];
int i;

    corn[0][X] = corn[3][X] = center[X] - siz/2.;
    corn[1][X] = corn[2][X] = center[X] + siz/2.;

    corn[0][Y] = corn[1][Y] = center[Y] - siz/2.;
    corn[2][Y] = corn[3][Y] = center[Y] + siz/2.;

    max = center[Z];
    if(!off){    
	for(i=0; i<4; i++){
	    tmp[X] = corn[i][X];
	    tmp[Y] = corn[i][Y];
	    if(viewcell_interp(tmp)){
		corn[i][Z] = (tmp[Z] - Zoff) * Z_exag;
		if (max < corn[i][Z])
		    max = corn[i][Z];
	    }
	}
    }

    corn[0][Z] = corn[1][Z] = corn[2][Z] = corn[3][Z] = max;

    cpack(co);
    linewidth(linew);
    bgnline();
    v3f(corn[0]);
    v3f(corn[1]);
    v3f(corn[2]);
    v3f(corn[3]);
    v3f(corn[0]);
    endline();

    return(max);

}


float
display_plane(center, siz, co, off)
float center[3], siz;
long co;
int off;
{
float max, corn[4][3];
double tmp[3];
int i;

    corn[0][X] = corn[3][X] = center[X] - siz/2.;
    corn[1][X] = corn[2][X] = center[X] + siz/2.;

    corn[0][Y] = corn[1][Y] = center[Y] - siz/2.;
    corn[2][Y] = corn[3][Y] = center[Y] + siz/2.;

    max = center[Z];

    if(!off){
	for(i=0; i<4; i++){
	    tmp[X] = corn[i][X];
	    tmp[Y] = corn[i][Y];
	    if(viewcell_interp(tmp)){
		corn[i][Z] = (tmp[Z] - Zoff) * Z_exag;
		if (max < corn[i][Z])
		    max = corn[i][Z];
	    }
	}
    }

    corn[0][Z] = corn[1][Z] = corn[2][Z] = corn[3][Z] = max;

    cpack(co);
    n3f(Ntop);
    bgnpolygon();
    v3f(corn[0]);
    v3f(corn[1]);
    v3f(corn[2]);
    v3f(corn[3]);
    endpolygon();

    return(max);
}

/* displays a line drawn cube 
at elevation of highest corner (or center if higher) of box */

float
display_cube(center, siz, co, linew, off)
float center[3], siz;
long co;
int linew, off;
{
float max, ftmp[3], corn[4][3];
double tmp[3];
int i;

    corn[0][X] = corn[3][X] = center[X] - siz/2.;
    corn[1][X] = corn[2][X] = center[X] + siz/2.;

    corn[0][Y] = corn[1][Y] = center[Y] - siz/2.;
    corn[2][Y] = corn[3][Y] = center[Y] + siz/2.;

    max = center[Z];
   
    if(!off){
	for(i=0; i<4; i++){
	    tmp[X] = corn[i][X];
	    tmp[Y] = corn[i][Y];
	    if(viewcell_interp(tmp)){
		corn[i][Z] = (tmp[Z] - Zoff) * Z_exag;
		if (max < corn[i][Z])
		    max = corn[i][Z];
	    }
	}
    }

    corn[0][Z] = corn[1][Z] = corn[2][Z] = corn[3][Z] = max;

    cpack(co);
    linewidth(linew);
    bgnline();
    v3f(corn[0]);
    v3f(corn[1]);
    v3f(corn[2]);
    v3f(corn[3]);
    v3f(corn[0]);
    endline();

    for(i=0; i<4; i++){
	ftmp[X] = corn[i][X];
	ftmp[Y] = corn[i][Y];
	ftmp[Z] = corn[i][Z];
	corn[i][Z] += siz * Z_exag/XYscale; 
	bgnline();
	v3f(corn[i]);
	v3f(ftmp);
	endline();
    }

    bgnline();
    v3f(corn[0]);
    v3f(corn[1]);
    v3f(corn[2]);
    v3f(corn[3]);
    v3f(corn[0]);
    endline();

    return(corn[0][Z]);  /* set for arrow display */

}


/* displays a solid cube 
at elevation of highest corner (or center if higher) of box */

float
display_solid_cube(center, siz, co, off)
float center[3], siz;
long co;
int off;
{
float max, tcorn[4][3], corn[4][3];
double tmp[3];
int i;


    corn[0][X] = corn[3][X] = center[X] - siz/2.;
    corn[1][X] = corn[2][X] = center[X] + siz/2.;

    corn[0][Y] = corn[1][Y] = center[Y] - siz/2.;
    corn[2][Y] = corn[3][Y] = center[Y] + siz/2.;

    max = center[Z];
    
    if(!off){
	for(i=0; i<4; i++){
	    tmp[X] = corn[i][X];
	    tmp[Y] = corn[i][Y];
	    if(viewcell_interp(tmp)){
		corn[i][Z] = (tmp[Z] - Zoff) * Z_exag;
		if (max < corn[i][Z])
		    max = corn[i][Z];
	    }
	}
    }

    corn[0][Z] = corn[1][Z] = corn[2][Z] = corn[3][Z] = max;

    for(i=0; i<4; i++){
	tcorn[i][X] = corn[i][X];
	tcorn[i][Y] = corn[i][Y];
	tcorn[i][Z] = corn[i][Z] + siz * Z_exag/XYscale;
    }

    cpack(co);

    n3f(Nsouth);
    bgnpolygon();
    v3f(corn[0]);
    v3f(corn[1]);
    v3f(tcorn[1]);
    v3f(tcorn[0]);
    endpolygon();

    n3f(Neast);
    bgnpolygon();
    v3f(corn[1]);
    v3f(corn[2]);
    v3f(tcorn[2]);
    v3f(tcorn[1]);
    endpolygon();

    n3f(Nnorth);
    bgnpolygon();
    v3f(corn[2]);
    v3f(corn[3]);
    v3f(tcorn[3]);
    v3f(tcorn[2]);
    endpolygon();

    n3f(Nwest);
    bgnpolygon();
    v3f(corn[3]);
    v3f(corn[0]);
    v3f(tcorn[0]);
    v3f(tcorn[3]);
    endpolygon();

    n3f(Ntop);
    bgnpolygon();
    v3f(tcorn[0]);
    v3f(tcorn[1]);
    v3f(tcorn[2]);
    v3f(tcorn[3]);
    endpolygon();

    return(tcorn[0][Z]);  /* set for arrow display */
}

display_north_arrow(cen, len, co)
float cen[3], len;
long co;
{
float v[4][3];
float base[2][3];


    base[0][Z] = base[1][Z] = v[0][Z] = v[1][Z] = v[2][Z] = v[3][Z] = cen[Z];
    base[0][X] = cen[X] - len/16.;
    base[1][X] = cen[X] + len/16.;
    base[0][Y] = base[1][Y] =  cen[Y] - len/2.;
    v[0][X] = v[2][X] = cen[X];
    v[1][X] = cen[X] + len/8.;
    v[3][X] = cen[X] - len/8.;
    v[0][Y] = cen[Y] + .2*len;
    v[1][Y] = v[3][Y] = cen[Y] + .1*len;
    v[2][Y] = cen[Y] + .5*len;

    n3f(Ntop);
    cpack(co);

    bgnpolygon();
    v3f(base[0]);
    v3f(base[1]);
    v3f(v[0]);
    endpolygon();

    bgnpolygon();
    v3f(v[0]);
    v3f(v[1]);
    v3f(v[2]);
    v3f(v[0]);
    endpolygon();
    bgnpolygon();
    v3f(v[0]);
    v3f(v[2]);
    v3f(v[3]);
    v3f(v[0]);
    endpolygon();
    
}


display_north_arrow3d(cen, len, co)
float cen[3], len;
long co;
{
float jnt[3], tip[3];
float wtip[4][3];
float base[4][3];


    base[0][Z] =base[2][Z] =jnt[Z] =tip[Z] =wtip[0][Z] =wtip[2][Z] =cen[Z];
    base[1][X] =base[3][X] =jnt[X] =tip[X] =wtip[1][X] =wtip[3][X] =cen[X];
    base[0][X] = cen[X] + len/16.;
    base[2][X] = cen[X] - len/16.;
    base[1][Z] = cen[Z] + len/16.;
    base[3][Z] = cen[Z] - len/16.;
    base[0][Y] = base[1][Y] = base[2][Y] = base[3][Y] = cen[Y] - len/2.;
    wtip[0][X] = cen[X] + len/8.;
    wtip[2][X] = cen[X] - len/8.;
    wtip[1][Z] = cen[Z] + len/8.;
    wtip[3][Z] = cen[Z] - len/8.;
    wtip[0][Y] = wtip[1][Y] = wtip[2][Y] = wtip[3][Y] = cen[Y] + .1*len;
    jnt[Y] = cen[Y] + .1*len;
    tip[Y] = cen[Y] + .5*len;

    cpack(co);
    
    n3f(Nsouth);
    bgnpolygon();
    v3f(base[0]);
    v3f(base[1]);
    v3f(base[2]);
    v3f(base[3]);
    endpolygon();

    bgnpolygon();
    n3f(Neast);
    v3f(base[0]);
    n3f(Nnorth);
    v3f(jnt);
    n3f(Ntop);
    v3f(base[1]);
    endpolygon();
    bgnpolygon();
    n3f(Ntop);
    v3f(base[1]);
    n3f(Nnorth);
    v3f(jnt);
    n3f(Nwest);
    v3f(base[2]);
    endpolygon();
    bgnpolygon();
    n3f(Nwest);
    v3f(base[2]);
    n3f(Nnorth);
    v3f(jnt);
    n3f(Nbottom);
    v3f(base[3]);
    endpolygon();
    bgnpolygon();
    n3f(Nbottom);
    v3f(base[3]);
    n3f(Nnorth);
    v3f(jnt);
    n3f(Neast);
    v3f(base[0]);
    endpolygon();

    bgnpolygon();
    n3f(Neast);
    v3f(wtip[0]);
    n3f(Ntop);
    v3f(wtip[1]);
    n3f(Nsouth);
    v3f(jnt);
    endpolygon();
    bgnpolygon();
    n3f(Ntop);
    v3f(wtip[1]);
    n3f(Nwest);
    v3f(wtip[2]);
    n3f(Nsouth);
    v3f(jnt);
    endpolygon();
    bgnpolygon();
    n3f(Nwest);
    v3f(wtip[2]);
    n3f(Nbottom);
    v3f(wtip[3]);
    n3f(Nsouth);
    v3f(jnt);
    endpolygon();
    bgnpolygon();
    n3f(Nbottom);
    v3f(wtip[3]);
    n3f(Neast);
    v3f(wtip[0]);
    n3f(Nsouth);
    v3f(jnt);
    endpolygon();

    bgnpolygon();
    n3f(Neast);
    v3f(wtip[0]);
    n3f(Nnorth);
    v3f(tip);
    n3f(Ntop);
    v3f(wtip[1]);
    endpolygon();
    bgnpolygon();
    n3f(Ntop);
    v3f(wtip[1]);
    n3f(Nnorth);
    v3f(tip);
    n3f(Nwest);
    v3f(wtip[2]);
    endpolygon();
    bgnpolygon();
    n3f(Nwest);
    v3f(wtip[2]);
    n3f(Nnorth);
    v3f(tip);
    n3f(Nbottom);
    v3f(wtip[3]);
    endpolygon();
    bgnpolygon();
    n3f(Nbottom);
    v3f(wtip[3]);
    n3f(Nnorth);
    v3f(tip);
    n3f(Neast);
    v3f(wtip[0]);
    endpolygon();
    
    
}


do_scale_obj_display(pt, offsurf)
float pt[3];
int offsurf;   /* pt is on zplane off of surface */
{
long co;
float z, maxz, arrow_size;
int arrow3d=0; 

    co = Scale_Color;
    if(!Aautoz->val){
	if(get_scalez(&z)){
	    pt[Z] = z;
	    offsurf = 1;
	}
	else{
	    Aautoz->val = 1;
	    pnl_fixact(Aautoz);
	}
    }
    maxz = pt[Z];

    if(A3dscale->val){
	if(Asolidscale->val){
	    lmcolor(LMC_DIFFUSE);  
	    maxz = display_solid_cube(pt, Scale_Size, co, offsurf);
	    co = ~(Scale_Color&0xCCCCCC);
	}
	if(Awirescale->val){
	    lmcolor(LMC_COLOR);   
	    maxz = display_cube(pt, Scale_Size, co, V_Width, offsurf);
	}
	put_scalez((float)(maxz - Scale_Size * Z_exag/XYscale));
    }

    else if(Aflatscale->val){
	zbuffer(0);
	if(Asolidscale->val){
	    lmcolor(LMC_DIFFUSE);  
	    maxz = display_plane(pt, Scale_Size, co, offsurf);
	    co = ~(Scale_Color&0xCCCCCC);
	}
	if(Awirescale->val){
	    lmcolor(LMC_COLOR);   
	    maxz = display_box(pt, Scale_Size, co, V_Width, offsurf);
	}
	put_scalez(maxz);
	zbuffer(1);
    }
    

    if(Asolidscale->val || Awirescale->val)
	arrow_size = Scale_Size * .667;
    else{ /* arrow only */
	arrow_size = Scale_Size;
	arrow3d = A3dscale->val;
    }

    if(Ashownorth->val){
	pt[Z] = maxz;
        if(Asolidscale->val) 
	    pt[Z] += Scale_Size * .02;
	if(arrow3d){
	    lmcolor(LMC_DIFFUSE);  
	    display_north_arrow3d(pt, arrow_size, co);
	}
	else{
	    lmcolor(LMC_COLOR);   
	    zbuffer(0);
	    display_north_arrow(pt, arrow_size, co);
	    zbuffer(1);
	}
    }

    lmcolor(LMC_COLOR);   
    linewidth (1);
    zbuffer(1);

}


display_ruler(x1, y1, x2, y2, z, ticdir, siz, co, drawup)
float x1,y1,x2,y2,z;
int drawup, ticdir;
double siz;
long co;
{
float start, end, ticlen, v1[3], v2[3];
int cnt, ticindex, rulindex, ruldir;

    
    ticlen = siz/5.;
    v1[X] = x1;
    v2[X] = x2;
    v1[Y] = y1;
    v2[Y] = y2;
    v1[Z] = v2[Z] = z;

    if(x1 == x2){
	ticindex = X;
	rulindex = Y;
    }
    else{
	ticindex = Y;
	rulindex = X;
    }

    ruldir = (v2[rulindex] > v1[rulindex]? 1: -1);

    cpack(co);
    linewidth(V_Width);
    zwritemask(0x0);

    bgnline();
    v3f(v1);
    v3f(v2);
    endline();

    start = v1[rulindex];
    end = v2[rulindex];
    
    v2[ticindex] = v1[ticindex] + ticdir*ticlen;

    cnt=0;
    if(ruldir == 1){
	while((v1[rulindex] = v2[rulindex] = start + cnt*siz) < end){
	    v1[Z] = v2[Z];
	    bgnline();
	    v3f(v2);
	    v3f(v1);
	    if(drawup){
		edge_interp(v1);
		v3f(v1);
	    }
	    endline();
	    cnt++;
	}
    }
    else{
	while((v1[rulindex] = v2[rulindex] = start - cnt*siz) > end){
	    v1[Z] = v2[Z];
	    bgnline();
	    v3f(v2);
	    v3f(v1);
	    if(drawup){
		edge_interp(v1);
		v3f(v1);
	    }
	    endline();
	    cnt++;
	}
    }

    linewidth(1);
    zwritemask(0xffffffff);

}

do_label_display(pt)
float pt[3];
{

	zbuffer(0);
	cpack(Label_Color);
	cmov(pt[X], pt[Y], pt[Z]);
	if(font_is_set())
	    fmprstr(PNL_ACCESS(Typein, Atext, str));
	else
	    charstr(PNL_ACCESS(Typein, Atext, str));
	zbuffer(1);

}



