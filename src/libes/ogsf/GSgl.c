/*  GS.c 
    Bill Brown, USACERL  
    June 1996
*/

#include "gis.h"
#include "gstypes.h"
#include "gsget.h"
#include <math.h>


#ifdef SGI_GL
#include "gl.h"
#elif USE_OGL
#include "GL/gl.h"
#endif

/***********************************************************************/
/* TODO: allow to set center? */

GS_init_view()
{
static int first = 1;

#ifdef TRACE_GS_FUNCS
Gs_status("GS_init_view");
#endif

#ifdef SGI_GL
    if(first){
	first = 0;
	mmode(MVIEWING);
	doublebuffer();
	lsetdepth (getgdesc(GD_ZMIN), getgdesc(GD_ZMAX));
	zbuffer(1);
    }
#elif USE_OGL
    if(first){
        first = 0;
        glMatrixMode(GL_MODELVIEW);
        /* OGLXXX doublebuffer: use GLX_DOUBLEBUFFER in attriblist */
        /* glxChooseVisual(*dpy, screen, *attriblist); */
        /* OGLXXX
         * ZMIN not needed -- always 0.
         * ZMAX not needed -- always 1.
         * getgdesc other posiblilties:
         *      glxGetConfig();
         *      glxGetCurrentContext();
         *      glxGetCurrentDrawable();
         * GLint gdtmp;
         * getgdesc other posiblilties:
         *      glxGetConfig();
         *      glxGetCurrentContext();
         *      glxGetCurrentDrawable();
         * GLint gdtmp;
         * glDepthRange params must be scaled to [0, 1]
         */
        glDepthRange(0.0, 1.0);
        glEnable(GL_DEPTH_TEST);
        glDepthFunc(GL_LEQUAL);
    }
#endif

    /* replace these with something meaningful */
    Gv.fov = 450;
    Gv.twist = 0;
    Gv.from_to[FROM][X] = Gv.from_to[FROM][Y] = 
			Gv.from_to[FROM][Z] = GS_UNIT_SIZE/2.;
    /*
    Gv.from_to[TO][X] = Gv.from_to[TO][Y] = Gv.from_to[TO][Z] = 0.;
    */
    Gv.from_to[TO][X] = GS_UNIT_SIZE/2.; 
    Gv.from_to[TO][Y] = GS_UNIT_SIZE/2.;
    Gv.from_to[TO][Z] = 0.;
    Gv.from_to[TO][W] = Gv.from_to[FROM][W] = 1.;

    Gv.real_to[W] = 1.;
    Gv.vert_exag = 1.;

    GS_v3eq(Gv.real_to, Gv.from_to[TO]);
    GS_v3normalize(Gv.from_to[FROM], Gv.from_to[TO]);

    Gd.nearclip = 50.;
    Gd.farclip = 10000.;
    Gd.aspect = (float) GS_get_aspect(); 

    GS_set_focus(Gv.real_to);

}


/***********************************************************************/
GS_clear(col)
int col;
{

#ifdef TRACE_GS_FUNCS
Gs_status("GS_clear");
#endif

#ifdef SGI_GL
    col = col | 0xFF000000;
    czclear(col, getgdesc(GD_ZMAX));
#elif USE_OGL
        col = col | 0xFF000000;
        /* OGLXXX
         * change glClearDepth parameter to be in [0, 1]
         * ZMAX not needed -- always 1.
         * getgdesc other posiblilties:
         *      glxGetConfig();
         *      glxGetCurrentContext();
         *      glxGetCurrentDrawable();
         * GLint gdtmp;
         */
    glClearDepth( 1.0 );
    glClearColor(((float)((col)&0xff))/255.,
                (float)((col)>>8&0xff)/255.,
                (float)((col)>>16&0xff)/255.,
                (float)((col)>>24&0xff)/255. );
    glClear(GL_DEPTH_BUFFER_BIT|GL_COLOR_BUFFER_BIT);

#endif

    Gd.bgcol = col;
    Modelshowing = 0;

}

/***********************************************************************/
double
GS_get_aspect()
{
int left, right, bottom, top;

#ifdef TRACE_GS_FUNCS
Gs_status("GS_get_aspect");
#endif

#ifdef SGI_GL

    getviewport (&left, &right, &bottom, &top);

#elif USE_OGL
        /* OGLXXX
         * get GL_VIEWPORT:
         * You can probably do better than this.
         */
    GLint tmp[4];

    glGetIntegerv(GL_VIEWPORT, tmp);
    left=tmp[0];
    right=tmp[0]+tmp[2]-1;
    bottom=tmp[1];
    top=tmp[1]+tmp[3]-1;

#endif

    return((double)(right-left) / (top-bottom));

}

GS_has_transparency()
{
#ifdef SGI

    return(getgdesc(GD_BLEND));
#elif USE_OGL

        /* OGLXXX
         * getgdesc other posiblilties:
         *      glxGetConfig();
         *      glxGetCurrentContext();
         *      glxGetCurrentDrawable();
         * GLint gdtmp;
         * blending is ALWAYS supported.
         * This function returns whether it is enabled.
         * return((glGetIntegerv(GL_BLEND, &gdtmp), gdtmp));
         */
    return(1);

#else

    return(0);

#endif
}
