#define MAIN

#include "graphics.h"

int SCREEN_LEFT	  ;
int SCREEN_RIGHT  ;
int SCREEN_BOTTOM ;
int SCREEN_TOP    ;
int NCOLORS    ;

Graph_Set() 
{
	int xleft,ybottom,xright,ytop,used ;

	static short cursor[] = {
	/*  (+) in a circle */
		0x07c0,
		0x1810,
		0x2008,
		0x4004,
		0x4004,
		0x8002,
		0x8002,
		0x8102,
		0x8002,
		0x8002,
		0x4004,
		0x4004,
		0x2008,
		0x1810,
		0x07c0,
		0x0000,
	};

	mgiasngp(0,0);
	mgiclearpln(2,-1,0);
	mgiloadcurs(7,7,512,cursor);	/* define cursor in plane 10 */
	mgigetvcoor (2, &xleft, &ybottom, &xright, &ytop, &used) ;
/* Define windows */
	mgidefw(FULL_SCREEN  );   /* Full screen window */
	mgipw(FULL_SCREEN  , 2, xleft,    ybottom,  xright,   ytop    ) ;
	SCREEN_LEFT   = xleft ;
	SCREEN_RIGHT  = xright ;
	SCREEN_BOTTOM = ytop ;
	SCREEN_TOP    = ybottom ;
	NCOLORS       = 512 ;

/* Set to use the MAP_WINDOW viewport first */
	mgiv(FULL_SCREEN);
	mgifb(1,3);
	mgimodfunc(3,0,0) ;

/* turn cursor off */
	/*
	printf("Gac") ;
	*/

	Set_plane_10_to_white() ;

/* Set text size land rotation value */
	Text_size(25, 25) ;
	Text_rotation(0.0) ;

/* set font */
	init_font("romant") ;
}

Set_plane_10_to_white()
{
	int mgi_INDEX[512] ;
	int white ;
	int incr ;

	white = -1 ;

	for (incr=0; incr<512; incr++)
		mgi_INDEX[incr] = white ;

	mgicms(512, 512, mgi_INDEX) ;
}

Set_plane_10_to_black()
{
	int mgi_INDEX[512] ;
	int black ;
	int incr ;

	black = 0 ;

	for (incr=0; incr<512; incr++)
		mgi_INDEX[incr] = black ;

	mgicms(512, 512, mgi_INDEX) ;
}
