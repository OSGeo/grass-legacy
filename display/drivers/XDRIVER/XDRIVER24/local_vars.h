/* variables used within XDRIVER */

extern const char *monitor_name;

extern Display *dpy;
extern Window grwin;

extern int screen_left, screen_right, screen_bottom, screen_top;
extern unsigned SC_WID, SC_HITE;
extern int NCOLORS;
extern Visual *use_visual;
extern int use_bit_depth;

extern int scrn;
extern GC gc;
extern Colormap floatcmap, fixedcmap;
extern Cursor cur_xh, cur_clock;
extern u_long gemask;
extern Pixmap bkupmap;
extern int truecolor;

extern unsigned long *xpixels;

extern int needs_flush;

extern pid_t redraw_pid;

