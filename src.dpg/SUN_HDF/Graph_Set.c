/*
 * Start up graphics processing.  Anything that needs to be assigned, set up,
 * started-up, or otherwise initialized happens here.  This is called only at
 * the startup of the graphics driver.
 *
 * The external variables define the pixle limits of the graphics surface.  The
 * coordinate system used by the applications programs has the (0,0) origin
 * in the upper left-hand corner.  Hence,
 *    SCREEN_LEFT < SCREEN_RIGHT
 *    SCREEN_TOP  < SCREEN_BOTTOM 
 *
 * NCOLORS is set to the total number of colors available on the device.  This
 * most certainly needs to be more than 100 (or so).  If you are writing a
 * driver with fewer colors you probably need to provide your own Color(),
 * Color_table_float(), Color_table_fixed(), and
 * Reset_color() routines (see ../lib/{Color.c,Reset_clr.c,Clr_table.c}).
 */

#define MAIN
#include "graphics.h"

static Frame base_frame;
static Canvas base_canvas;
static Rect framerect ;
static short icon_image[] = {
#include "grass.icon"
} ;
DEFINE_ICON_FROM_IMAGE(grass_icon, icon_image) ;
static PIXFONT *font ;
#define font_offset(font)	(-font->pf_char['n'].pc_home.y)
#define font_height(font)	(font->pf_defaultsize.y)

do_main_loop()
{
        window_main_loop(base_frame);
}

Graph_Set() 
{
        char *getenv() ;
	char *string ;
        int stripeheight ;
	static Notify_value catch_resize() ;
        Rect *r ;
	int width ;
	int height ;

        if (!getenv("WINDOW_ME")) {
                printf("Must be running in Suntools\n");
                exit(-1);
        }
        if(getenv("GRASS_COLOR256"))
                CTLEN = 256;
        else
                CTLEN = 128;
        CTMAX = CTLEN - 1;

	height = 896 ;
	width = 1152 ;
	if(string = getenv("GRASS_WIDTH"))
	{
		sscanf(string,"%d", &width) ;
		if (width < 100)
			width = 100 ;
		if (width > 1152)
			width = 1152 ;
	}
	if(string = getenv("GRASS_HEIGHT"))
	{
		sscanf(string,"%d", &height) ;
		if (height < 100)
			height = 100 ;
		if (height > 896)
			height = 896 ;
	}

	N_COLORS = CTLEN;

        base_frame = window_create(NULL, FRAME,
                        WIN_X,          0,
                        WIN_Y,          0,
                        WIN_HEIGHT,     height,
                        WIN_WIDTH,      width,
                        FRAME_LABEL,    "GRASS GRAPHICS WINDOW",
			FRAME_ICON,	&grass_icon,
			WIN_ERROR_MSG,	"Can't create GRASS graphics",
                        0);
        r = (Rect *) window_get(base_frame, WIN_RECT) ;
        framerect = *r ;
	stripeheight = (int) window_get(base_frame, WIN_TOP_MARGIN) ;

	make_canvas(framerect.r_width - stripeheight, framerect.r_height - stripeheight) ;

	font = pf_default() ;

        notify_interpose_event_func(base_frame, catch_resize, NOTIFY_SAFE) ;
	resize(base_frame) ;

        return(0) ;
}

resize(frame)
        Frame frame ;
{
        Rect *r ;
        int canvas_width ;
        int stripeheight ;
 
        if ((int)window_get(frame,FRAME_CLOSED))
                return ;
        r = (Rect *) window_get(frame, WIN_RECT) ;
        framerect = *r ;
	stripeheight = (int) window_get(frame, WIN_TOP_MARGIN) ;

	make_canvas(framerect.r_width - stripeheight, framerect.r_height - stripeheight) ;
}

static Notify_value
catch_resize(frame, event, arg, type)
        Frame frame ;
        Event *event ;
        Notify_arg arg ;
        Notify_event_type type ;
{
        Notify_value value ;
        value = notify_next_event_func(frame, event, arg, type) ;
        if (event_id(event) == WIN_RESIZE)
                resize(frame) ;
        return(value) ;
}

make_canvas(width, height)
	int width, height ;
{
        int planes;
	char buf[128] ;
        int i ;
        unsigned char *R, *G, *B ;
	static int first = 1 ;
        Cursor cursor;

/* Establish arrays for setting first colors */
	R = (unsigned char *)malloc(N_COLORS) ;
	G = (unsigned char *)malloc(N_COLORS) ;
	B = (unsigned char *)malloc(N_COLORS) ;
	if (R == NULL || B == NULL | G == NULL)
	{
		fprintf(stderr, "Insufficient memory in Graph_Set\n") ;
		exit (-1) ;
	}

	if (first)
	{
	/* Set color map to grey for a start */
		for(i=0 ; i< N_COLORS; i++)
			R[i] = G[i] = B[i] = i ;
		first = 0 ;
	}
	else
	{
	/* Save current color map for next incarnation of pixwin */
		pw_getcolormap(pixwin, 0, N_COLORS, R, G, B) ;
		R[0] = G[0] = B[0] = 0 ;
	/* Destroy current canvas */
		window_destroy(base_canvas) ;
	}
		
        base_canvas = window_create(base_frame, CANVAS,
		CANVAS_RETAINED, TRUE,
		CANVAS_FIXED_IMAGE, FALSE,
		CANVAS_AUTO_CLEAR,  TRUE,
		0);
        window_set(base_canvas,
                WIN_X,  0,
                WIN_Y,  0,
                WIN_WIDTH,      width + 9,
                WIN_HEIGHT,     height - 5,
                0) ;

        SCREEN_LEFT   = 0;
        SCREEN_RIGHT  = width + 8 ;
        SCREEN_BOTTOM = height - 6 ;
        SCREEN_TOP    = 0;

	pixwin = canvas_pixwin(base_canvas);
	pw_getattributes(pixwin, &planes);

        cursor = cursor_copy(window_get(base_canvas, WIN_CURSOR, 0));
        cursor_set(cursor, CURSOR_OP, PIX_SRC ^ PIX_DST, 0);
        window_set(base_canvas, WIN_CURSOR, cursor, 0);

	if (planes > 0)
		has_color = TRUE;
	else
		has_color = FALSE;

	/* the first call to pw_putcolormap allocates as much color space
	 * as needed for that call.  Subsequent calls can only change colors 
	 * in this established range.
	 */
        pw_setcmsname(pixwin, "grass") ;
        pw_putcolormap(pixwin, 0, N_COLORS, R, G, B) ;
        free(R) ;
        free(G) ;
        free(B) ;

	clear_all_pads() ;

	font = pf_default() ;
	sprintf(buf,"%d by %d pixles", SCREEN_RIGHT, SCREEN_BOTTOM) ;
	pw_text(pixwin, 5, font_offset(font), PIX_SRC, font, 
		"Resized graphics window ") ;
	pw_text(pixwin, 5, font_offset(font) + font_height(font),
		PIX_SRC, font, buf) ;
}
