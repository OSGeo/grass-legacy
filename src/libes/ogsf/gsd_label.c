
/*
  Converting this SG3d code to gsf42
                          - mark
 */

/*
**  Written by Bill Brown, Winter 1991 - 1992 
**  US Army Construction Engineering Research Lab
*/

/*
** Copyright USA CERL 1992. All rights reserved.
*/

/* #include "externs.h" */
#include "math.h"
/*
#include "device.h"
#include "gis.h"
*/
#include "gstypes.h"

#ifdef SGI_GL
#include "gl.h"
#elif USE_OGL
#include "GL/gl.h"
#endif


#ifdef SAVE_UNDER
static long *save_buf;
static long sav_siz=0, sav_l, sav_b, sav_r, sav_t;

save_under(l, b, r, t)
Screencoord l, b, r, t; 
{
long size;
static int first=1;

#ifdef OLD_SGI_GL

    if(first){
	first = 0;
	sav_siz = 200 * 500;
	if(NULL == (save_buf = (long *)malloc(sav_siz * sizeof(long)))){
	    fprintf(stderr,"out of memory\n");
	    exit(1);
	}
    }
    
    size = (1 + r-l)*(1 + t-b);
    if(size > sav_siz){
	if(save_buf)
	    free(save_buf);
	if(NULL == (save_buf = (long *)malloc(size * sizeof(long)))){
	    fprintf(stderr,"out of memory\n");
	    exit(1);
	}
	sav_siz = size;
    }
    lrectread (l, b, r, t, save_buf); 
    sav_l = l;
    sav_b = b;
    sav_r = r;
    sav_t = t;

#endif
}

restore_under()
{
#ifdef OLD_SGI_GL

	    if(!save_buf) return;

#ifdef FOUR_OH
	    dither(DT_OFF);
#endif  /* FOUR_OH */

	    frontbuffer(1);
	    lrectwrite (sav_l, sav_b, sav_r, sav_t, save_buf); 
	    frontbuffer(0);

#ifdef FOUR_OH
	    dither(DT_ON);
#endif  /* FOUR_OH */

#endif
}


save_under_label()
{
long  ox, oy;

#ifdef OLD_SGI_GL
Screencoord labx, laby, labw, labh;

    getorigin(&ox, &oy);
    getviewport (&left, &right, &bottom, &top);
    ox += left;
    oy += bottom;
    labx = getvaluator(MOUSEX) - ox - get_txtxoffset();
    laby = getvaluator(MOUSEY) - oy - get_txtdescender();
    labw = get_txtwidth(PNL_ACCESS(Typein, Atext, str)) + get_txtxoffset();
    labh = get_txtheight();

    save_under(labx, laby, labx+labw, laby+labh);

#endif
}
#endif  /* save_under */

/* This is messier than it has to be - BB */
/* Don't need to do a get_los, just need to 
   pushviewport, viewport & ortho2 - these will be done
   in a gsd_prim.c routine since they use GL */

void gs_put_label(short sx, short sy, char *theLabel)
{
#ifdef OLD_SGI_GL
  Screencoord left, right, bottom, top;
  float width, height;
  
/*  save_under_label(); */

  /* Push the current viewport */
  /* pushviewport(); */

  /* Get current viewport */
  getviewport(&left, &right, &bottom, &top);
  width = right - left + 1;
  height = top - bottom + 1;
  fprintf(stdout,"Screen coords: %f %f\n", width, height);
  
  /* Remap viewport to do drawing */
  /*  viewport(0, (Screencoord)(width - 1), 0, (Screencoord)(height - 1)); */
  /*  ortho2(-0.5, width - 0.5, -0.5, height - 0.5);
  reshapeviewport(); */
  
  /* Turn off Z buffering and make sure we write to the visible frame buffer */
  /*  zbuffer(0);  */
  frontbuffer(TRUE); 
    
/*	cpack(Label_Color); */
  cpack(0xFFFFFF);

  fprintf(stdout,"Drawing string at: %d %d\n", sx, sy);
  cmov2((Coord)sx, (Coord)sy);
  { short ix, iy;
  getcpos(&ix, &iy);
  fprintf(stdout,"Char pos: %d %d\n",ix,iy);
  }
  
  if(font_is_set())
    fmprstr(theLabel);
  else 
    charstr(theLabel);

  frontbuffer(FALSE);
  /*  zbuffer(1); */

  fprintf(stderr,"<label set>\n");

  /* Restore viewport */
  /* popviewport(); */
#endif
}

void
undo_label()
{
#ifdef SAVE_UNDER
    restore_under();
#endif

}

#ifdef CENTROID
get_vis_pt_on_los(pt, los)
float pt[4], los[2][4];
{
float dir[3], len, cent[3];

    get_norm_direction(dir, los);
    get_centroid(cent);
    len = distance(los[FROM], cent);
    pt[X] = los[FROM][X] + dir[X] * len;
    pt[Y] = los[FROM][Y] + dir[Y] * len;
    pt[Z] = los[FROM][Z] + dir[Z] * len;

}

#endif


