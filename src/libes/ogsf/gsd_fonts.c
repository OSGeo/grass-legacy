/*
* $Id$
*
****************************************************************************
*
* MODULE: 	GRASS ogsf library
* AUTHOR(S):	none
* PURPOSE: 	This file needs to be re-written in OpenGL
* COPYRIGHT:    (C) 2000 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/


#include "gstypes.h"
#include "rgbpack.h"
#include <assert.h>
#include "local_proto.h"

/* X11 stuff */
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#include "GL/glx.h"

#define MAX_FONTS 1000
static GLuint ListBase[MAX_FONTS];
static GLuint ListCount[MAX_FONTS];

static XFontStruct *fontinfo;


GLuint gsd_set_font(const char *fontname)
{

static int FirstTime = 1;
int first, last, count;
GLuint fontbase;
const char *name;
Display *dpy;


   /* Initialize the ListBase and ListCount arrays */
   if (FirstTime) {
      int i;
      for (i=0;i<MAX_FONTS;i++) {
         ListBase[i] = ListCount[i] = 0;
      }
      FirstTime = 0;
   }

   /* Get fontaname from arg -- maybe should define default font if
    * arg is unavialable 
    */
      name = (const char *) fontname;

   assert( name );

   dpy = glXGetCurrentDisplay();
   fontinfo = XLoadQueryFont( dpy, name );
   if (!fontinfo) {
     fprintf(stderr, "Error -- unable to load font\n");
      return 0;
   }

   first = fontinfo->min_char_or_byte2;
   last = fontinfo->max_char_or_byte2;

   count = last-first+1;

   fontbase = glGenLists( (GLuint) (last+1) );
   if (fontbase==0) {
	return 0;
   }

glXUseXFont( fontinfo->fid, first, count, (int) fontbase+first );


   {
      int i;
      for (i=0;i<MAX_FONTS;i++) {
         if (ListBase[i]==0) {
            ListBase[i] = fontbase;
            ListCount[i] = last+1;
            break;
         }
      }
   }

   return fontbase;
}

/****************************************/
int
gsd_get_txtwidth(char *s) {
int width, len;

  len = strlen(s);
  width = XTextWidth(fontinfo, s, len);

  return(width);
}


/*****************************************/
int
gsd_get_txtheight() {
unsigned long height;

XGetFontProperty(fontinfo, XA_X_HEIGHT , &height);

return(height);
}


/*****************************************/
int
get_txtdescender() {

/* yorig ?? 
 * Is this defined somewhere ?
 */
	
  return(2);
}

/*****************************************/
int
get_txtxoffset() {

/* xorig ??
 * Is this defined somewhere ?
 */
  
  return(0);
}

/*****************************************/
void
do_label_display(GLuint fontbase, float *lab_pos, char *txt)
{
unsigned long colr;
unsigned int first, last;

  glRasterPos2f(lab_pos[X], lab_pos[Y]);
  glListBase(fontbase);
  glCallLists(strlen(txt), GL_BYTE, (GLubyte *)txt);

return;
}

/*****************************************/
void
gsd_unset_font(GLuint fontbase)
{
int i;
for (i=0;i<MAX_FONTS;i++) {
      if (ListBase[i]==fontbase) {
         glDeleteLists( ListBase[i], ListCount[i] );
         ListBase[i] = ListCount[i] = 0;
         return;
      }
   }
}



