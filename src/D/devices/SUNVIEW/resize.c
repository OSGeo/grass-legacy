#include <suntool/sunview.h>
#include <suntool/canvas.h>
#include <suntool/scrollbar.h>

Canvas canvas ;
Pixwin *pixwin ;
Rect framerect ;
PIXFONT *font ;

extern char * sprintf() ;

#define font_offset(font)	(-font->pf_char['n'].pc_home.y)
#define font_height(font)	(font->pf_defaultsize.y)

#define LEFT_MARGIN	5
#define RIGHT_MARGIN	5
#define BOTTOM_MARGIN	5

#define CANVAS_WIDTH	320
#define CANVAS_HEIGHT	160
#define CANVAS_COLUMNS	30

main(argc, argv)
int argc ;
char **argv ;
{
	Frame frame ;
	static Notify_value catch_resize() ;
	static void draw_canvas() ;

	frame = window_create(NULL, FRAME, FRAME_ARGS, argc, argv,
		WIN_ERROR_MSG, "Can't create tool frame",
		FRAME_LABEL, "Resize Demo",
		0) ;
	canvas = window_create(frame, CANVAS,
		CANVAS_RESIZE_PROC, draw_canvas,
		0) ;
	pixwin = canvas_pixwin(canvas) ;

	font = pf_default() ;

	resize(frame) ;

	(void) notify_interpose_event_func(frame, catch_resize, NOTIFY_SAFE) ;

	window_main_loop(frame) ;
	exit(0) ;
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

	window_set(canvas,
		WIN_X,	0,
		WIN_Y,	framerect.r_height - CANVAS_HEIGHT - 5 - stripeheight,
		WIN_WIDTH,	CANVAS_WIDTH,
		WIN_HEIGHT,	CANVAS_HEIGHT,
		0) ;
}

static void
draw_canvas()
{
	char buf[64] ;
	sprintf(buf,"%d by %d pixles", 320, 160) ;
	pw_text(pixwin, 5, font_offset(font), PIX_SRC, font, buf) ;
}
