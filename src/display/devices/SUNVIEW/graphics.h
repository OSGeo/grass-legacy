#include <stdio.h>
#include <suntool/sunview.h>
#include <suntool/canvas.h>
#include <sunwindow/notify.h>
#include <sunwindow/pixwin.h>

#ifdef MAIN
#define GLOBAL
#else
#define GLOBAL extern ;
#endif

#define iMAXINTENSITY   255
#define fMAXINTENSITY   255.0

GLOBAL int CTLEN;
GLOBAL int CTMAX;

GLOBAL int SCREEN_LEFT ;
GLOBAL int SCREEN_RIGHT ;
GLOBAL int SCREEN_BOTTOM ;
GLOBAL int SCREEN_TOP ;
GLOBAL int N_COLORS ;
GLOBAL double _text_size_x ;
GLOBAL double _text_size_y ;
GLOBAL double _text_rotation ;
GLOBAL Pixwin *pixwin;
GLOBAL Canvas base_canvas;
GLOBAL struct pr_brush *Brush;
GLOBAL struct pr_texture *Texture;
GLOBAL int Op;
GLOBAL int has_color;
