/* Header file: vdc600.h
**
** This file contains the constants, macros, and global variables
** used in the library functions that use the /dev/console ioctl(2)
** calls defined in release 3.2.
**
** Author: Paul W. Carlson		Jan. 1990
*/
/************** Constants ***********************************************/
#define ATC_IND_SEL	0x03C0	/* ATC index select register		*/
#define ATC_IND_REG	0x03C0	/* ATC indexed register			*/
#define WR_MISC_OUT	0x03C2	/* Misc. output register (write)	*/
#define TS_IND_SEL	0x03C4	/* TS index select register		*/
#define TS_IND_REG	0x03C5	/* TS indexed register			*/
#define PAL_MASK	0x03C6	/* Palette mask register		*/
#define PAL_READ	0x03C7	/* Palette read register		*/
#define PAL_WRITE	0x03C8	/* Palette write register		*/
#define PAL_DATA	0x03C9	/* Palette data register		*/
#define RD_MISC_OUT	0x03CC	/* Misc. output register (read)		*/
#define GDC_SEG_SEL	0x03CD	/* GDC segment select register		*/
#define GDC_IND_SEL	0x03CE	/* GDC index select register		*/
#define GDC_IND_REG	0x03CF	/* GDC indexed register			*/
#define CRTC_IND_SEL	0x03D4	/* CRTC index select register		*/
#define CRTC_IND_REG	0x03D5	/* CRTC indexed register		*/
#define MODE_CONTROL	0x03D8	/* Display mode control register	*/
#define INP_STAT_1	0x03DA	/* Input status register one		*/
#define H_RES		640	/* horizontal display resolution	*/
#define V_RES		400	/* vertical display resolution		*/
#define DACMAP		('D' << 8)
#define DAC_WRITE	(DACMAP | 1)
#define DAC_READ 	(DACMAP | 2)
#define DAC_PALS 	(DACMAP | 3)
#define DAC_SHIFT 	(DACMAP | 4)
#define MOUSE_INIT	(('M' << 8) | 81)
#define GET_MOUSE	(('M' << 8) | 82)



/*************** Macros *************************************************/
#define set_color(COLOR) \
{ \
    /* this macro sets the current color number */ \
    current.color = (unsigned char)COLOR; \
}

#define CHECK_SEG() \
{   /* this macro updates the video segment if necessary */ \
    if (old_seg != video.segment.number) \
    {	set_seg.args[3].data = 16 * video.segment.number; \
	ioctl(vidfd, EGAIO, &set_seg); \
	old_seg = video.segment.number; \
    } \
}

#define setpixel(X, Y) \
{ \
    /* this macro plots a pixel in the current color */ \
    video.total_offset = H_RES * (Y) + X; \
    CHECK_SEG(); \
    *(graphics_base + video.segment.offset) = current.color; \
}

#define write_dac(DAC_REG, RED_INT, GRN_INT, BLU_INT) \
{   /* this macro writes the RGB intensities to a DAC register */ \
    dac_args.arg1 = DAC_REG; \
    dac_args.arg2 = RED_INT; \
    dac_args.arg3 = GRN_INT; \
    dac_args.arg4 = BLU_INT; \
    ioctl(dacfd, DAC_WRITE, &dac_args); \
}

#define read_dac(DAC_REG, RED_INT, GRN_INT, BLU_INT) \
{   /* this macro reads the RGB intensities of a DAC register */ \
    dac_args.arg1 = DAC_REG; \
    ioctl(dacfd, DAC_READ, &dac_args); \
    RED_INT = dac_args.arg2; \
    GRN_INT = dac_args.arg3; \
    BLU_INT = dac_args.arg4; \
}


#define set_palette(PAL) \
{   /* selects one of the 5 palettes */ \
    dac_args.arg1 = PAL; \
    ioctl(dacfd, DAC_PALS, &dac_args); \
}

#define shift_dac() \
{   /* shifts the contents of the DAC registers */ \
    ioctl(dacfd, DAC_SHIFT, &dac_args); \
}

#define SEL_GDC(GDC_REG) \
{   /* this macro selects a GDC indexed register */ \
    sel_gdc.args[0].data = GDC_REG; \
    ioctl(vidfd, EGAIO, &sel_gdc); \
}

#define OUT_GDC(GDC_BYTE) \
{   /* this macro outputs a byte to a GDC indexed register */ \
    out_gdc.args[0].data = GDC_BYTE; \
    ioctl(vidfd, EGAIO, &out_gdc); \
}

#define SEL_TS(TS_REG) \
{   /* this macro selects a TS indexed register */ \
    sel_ts.args[0].data = TS_REG; \
    ioctl(vidfd, EGAIO, &sel_ts); \
}

#define OUT_TS(TS_BYTE) \
{   /* this macro outputs a byte to a TS indexed register */ \
    out_ts.args[0].data = TS_BYTE; \
    ioctl(vidfd, EGAIO, &out_ts); \
}

/*************** Global Variables ***************************************/
#ifdef GRAPH_SET
#define GLOBAL
unsigned char bit_val[] = { 128, 64, 32, 16, 8, 4, 2, 1 };
unsigned char red_int[] = {
	 0,  0,  0,  0, 42, 42, 42, 42, 21, 21, 21, 21, 63, 63, 63, 63 };
unsigned char grn_int[] = {
	 0,  0, 42, 42,  0,  0, 21, 42, 21, 21, 63, 63, 21, 21, 63, 63 };
unsigned char blu_int[] = {
	 0, 42,  0, 42,  0, 42,  0, 42, 21, 63, 21, 63, 21, 63, 21, 63 };
unsigned char color_int[] = { 0, 13, 25, 38, 50, 63 };
#else
#define GLOBAL	extern
extern unsigned char bit_val[];
extern unsigned char red_int[];
extern unsigned char grn_int[];
extern unsigned char blu_int[];
extern unsigned char color_int[];
#endif

/* ---------- uninitialized arrays --------------------------------*/
GLOBAL unsigned char raster_buff[800];	/* raster color array */
GLOBAL unsigned char screen[4000];	/* saved text display */

/*----------- Simple Variables ----------------------------------*/
GLOBAL int vidfd;		/* /dev/console file descriptor */
GLOBAL int dacfd;		/* /dev/dac file descriptor */
GLOBAL int vid_is_open;		/* /dev/console flag */
GLOBAL int dac_is_open;		/* /dev/dac flag */
GLOBAL int mousfd;		/* /dev/mouse file descriptor */
GLOBAL int mouse_is_open;	/* /dev/mouse flag */
GLOBAL int start_x, start_y;	/* starting coords. */
GLOBAL int end_x, end_y;		/* ending coords. */
GLOBAL int bytes_per_line;	/* video buffer width in bytes */
GLOBAL unsigned short old_seg;	/* old video segment number */
GLOBAL char *graphics_base;	/* graphics base address */
GLOBAL char *text_base;		/* text base address */
GLOBAL char *save_gbase;		/* saved graphics base address */
GLOBAL unsigned char cursor_r;	/* text cursor row */
GLOBAL unsigned char cursor_c;	/* text cursor column */
GLOBAL unsigned char attrib_byte;/* text screen attribute byte */

GLOBAL int mse_x1, mse_y1, mse_x2, mse_y2;
GLOBAL unsigned char hor_pix[26], ver_pix[26];
GLOBAL unsigned int mse_voff;
GLOBAL unsigned char box_top[H_RES], box_bot[H_RES], 
		     box_lt[V_RES],  box_rt[V_RES];
GLOBAL int mse_old_dx, mse_old_dy, mse_xinc, mse_yinc;
GLOBAL mse_err, mse_errinc, mse_errdec;
GLOBAL unsigned char *mse_pix_ptr, mse_pixels[H_RES];
GLOBAL double _text_size_x;		/* text width */
GLOBAL double _text_size_y;		/* text heught */
GLOBAL double _text_rotation;		/* text rotation */
GLOBAL int SCREEN_LEFT;			/* minimum x coordinate */
GLOBAL int SCREEN_TOP;			/* minimum y coordinate */
GLOBAL int SCREEN_RIGHT;		/* maximum x coordinate */
GLOBAL int SCREEN_BOTTOM;		/* maximum y coordinate */
GLOBAL int NCOLORS;			/* number of colors */

/*------------- Unions ------------------------------------*/
GLOBAL union {
    unsigned int total_offset;	/* offset from start of video memory */
    struct {
    unsigned short offset;	/* offset from start of video segment */
    unsigned short number;	/* video segment number */
    } segment;
} video;

/*------------- Structures ----------------------------------*/
GLOBAL struct dac_struct {	/* used for ioctl calls to /dev/dac */
    int arg1;
    int arg2;
    int arg3;
    int arg4;
} dac_args;

GLOBAL struct {
    int x;			/* current x coordinate */
    int y;			/* current y coordinate */
    int color;			/* current plotting color */
} current;

GLOBAL struct {
    int old_x;			/* old mouse cursor x coordinate */
    int old_y;			/* old mouse cursor y coordinate */
    int fixed_x;		/* fixed x coord. of box or line */
    int fixed_y;		/* fixed y coord. of box or line */
    int float_x;		/* floating x coordinate */
    int float_y;		/* floating y coordinate */
} mouse;

GLOBAL struct kd_memloc		/* kd.h video memory structure */
	graphics_mem;		/* used to allocate graphics memory */

GLOBAL struct port_io_arg	/* kd.h port io structure */
	set_seg,		/* used to update the video segment */
	sel_gdc,		/* used to select any GDC index register */
	out_gdc,		/* used to ouput a byte to a GDC register */
	sel_ts,			/* used to select any TS register */
	out_ts,			/* used to output a byte to a TS register */
	vwait;			/* used to wait for vertical retrace */

