#include "vizual.h"
#include <device.h>

do_lights()
{
  static float white_light1[]={
      LCOLOR, 0.58, 0.58, 0.58,
      POSITION, -150.0, 100.0,200.0, 0.0,
      LMNULL};

  static float white_light2[]={
      LCOLOR, 0.40, 0.40, 0.40,
      POSITION, 150.0, 40.0, -200.0, 0.0,
      AMBIENT, 0.3,0.3,0.3,
      LMNULL};
  static float material[]={
      SPECULAR,1.0, 1.0, 1.0,
      DIFFUSE, 0.8, 0.8, 0.8,
      AMBIENT, 0.8, 0.8, 0.8,
      SHININESS, 10,
      LMNULL};


  lmdef(DEFLIGHT,1, 10, white_light1);
  lmdef(DEFLIGHT, 2, 14, white_light2);
  lmdef(DEFLMODEL,1,0,NULL);
  lmdef(DEFMATERIAL,1, 15, material);

  lmbind(LIGHT0,1); 
  lmbind(LIGHT1,2);
  lmbind(LMODEL,1);
  lmbind(MATERIAL,1);

  lmcolor (LMC_AD);
}


/************************************  make_matcolors  ************************/
/* this code came from GRASS color code written by M.Shapiro USACERL */
draw_colortable (D_spec, Headfax, Window)
    struct dspec *D_spec;
    file_info *Headfax;
    long Window[];
{
    int max; /* the number of colors in the colortable = number of thresholds*/
    short t,z;
    short x1,x2,y1,y2;
    short vert[4][2]; /* the vertices of the colorsquares */
    int yadd;
    long tmp_wid;
    short color[3];

    x1 = 10; x2 = 60;
    y1 = y2 = 0;

    tmp_wid = winget ();

    
    winset (Window[2]);

    yadd = (int)1000/Headfax->linefax.nthres;

    /* draw the colortable */
    for(t = 0; t < Headfax->linefax.nthres; t++)
    {
	y1 = y2;
	y2+= yadd;

	vert[0][0] = x1; vert[0][1] = y1;
	vert[1][0] = x2; vert[1][1] = y1;
	vert[2][0] = x2; vert[2][1] = y2;
	vert[3][0] = x1; vert[3][1] = y2;
        
	get_cat_color (Headfax->linefax.tvalue[t], D_spec->ctable, color);
	c3s (color);
	bgnpolygon();
	     v2s(vert[0]);
	     v2s(vert[1]);
	     v2s(vert[2]);
	     v2s(vert[3]);
	endpolygon();
    }
    winset (tmp_wid);
}

