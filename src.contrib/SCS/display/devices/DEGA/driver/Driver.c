/* This file contains the source code for the EGA video card device driver.
 *
 * Author: Paul W. Carlson    Jan. 1989
 */

static char *version = "1.03";

/*======== system include files ===============================*/
#include <sys/types.h>
#include <sys/sysmacros.h>
#include <sys/param.h>
#include <sys/systm.h>
#include <sys/dir.h>
#include <sys/signal.h>
#include <sys/user.h>
#include <sys/errno.h>
#include <sys/immu.h>
 
/*======== character map include file ==========================*/
#include "charmap.h"

/*======== EGA/VGA register definitions =======================*/
#define HERC_COMP	0x03BF	/* Hercules compatibility reg. */
#define ATC_IND_SEL	0x03C0	/* ATC index select register */
#define ATC_IND_REG	0x03C0	/* ATC indexed register */
#define WR_MISC_OUT	0x03C2	/* Misc. output register (write) */
#define TS_IND_SEL	0x03C4	/* TS index select register */
#define TS_IND_REG	0x03C5	/* TS indexed register */
#define PAL_MASK	0x03C6	/* Palette mask register */
#define PAL_READ	0x03C7	/* Palette read register */
#define PAL_WRITE	0x03C8	/* Palette write register */
#define PAL_DATA	0x03C9	/* Palette data register */
#define RD_MISC_OUT	0x03CC	/* Misc. output register (read) */
#define GDC_SEG_SEL	0x03CD	/* GDC segment select register */
#define GDC_IND_SEL	0x03CE	/* GDC index select register */
#define GDC_IND_REG	0x03CF	/* GDC indexed register */
#define CRTC_IND_SEL	0x03D4	/* CRTC index select register */
#define CRTC_IND_REG	0x03D5	/* CRTC indexed register */
#define MODE_CONTROL	0x03D8	/* Display mode control register */
#define INP_STAT_1	0x03DA	/* Input status register one */

#define Set_Reset_Mode() \
{ \
	outb(GDC_IND_SEL, 0x05); \
	outb(GDC_IND_REG, 0x00); \
	outb(GDC_IND_SEL, 0x03); \
	outb(GDC_IND_REG, 0x00); \
	outb(GDC_IND_SEL, 0x01); \
	outb(GDC_IND_REG, 0x0F); \
	outb( TS_IND_SEL, 0x02); \
	outb( TS_IND_REG, 0x0F); \
}

#define H_RES	640		/* horizontal display resolution */
#define V_RES	350		/* vertical display resolution */

static unsigned char G_TSvals[] = {
	0x02, 0x03, 0x0f, 0x00, 0x06, 0x00, 0x00, 0x88 };

static unsigned char G_CRTvals[] = {
	0x5f, 0x4f, 0x50, 0x02, 0x54, 0x80, 0xbf, 0x1f,
	0x00, 0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0xa0,
	0x83, 0x85, 0x5d, 0x28, 0x0f, 0x63, 0xba, 0xe3,
	0xff, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00 };

static unsigned char G_ATCvals[] = {
	0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x14, 0x07,
	0x38, 0x39, 0x3a, 0x3b, 0x3c, 0x3d, 0x3e, 0x3f,
	0x01, 0x00, 0x0f, 0x00, 0x00, 0x00, 0x00 };

static unsigned char G_GDCvals[] = {
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x05, 0x0f, 0xff };

static unsigned char A_TSvals[] = {
	0x03, 0x01, 0x03, 0x00, 0x02, 0x00, 0x00, 0x88 };

static unsigned char A_CRTvals[] = {
	0x5f, 0x4f, 0x50, 0x02, 0x55, 0x81, 0xbf, 0x1f,
	0x00, 0x4d, 0x0b, 0x0c, 0x00, 0x00, 0x00, 0x00,
	0x83, 0x85, 0x5d, 0x28, 0x1f, 0x63, 0xba, 0xa3,
	0xff, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00 };

static unsigned char A_ATCvals[] = {
	0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x14, 0x07,
	0x38, 0x39, 0x3a, 0x3b, 0x3c, 0x3d, 0x3e, 0x3f,
	0x08, 0x00, 0x0f, 0x00, 0x00, 0x00, 0x00 };

static unsigned char A_GDCvals[] = {
	0x00, 0x00, 0x00, 0x00, 0x00, 0x10, 0x0e, 0x00, 0xff };

/*============= ioctl functions and structure =============*/
void	ega_draw(),	ega_move(),	ega_putrast(),	ega_setcolor(),
	ega_setpix(),	ega_getpix(),	ega_horline(),	ega_erase(),
	ega_putpal(),	ega_raster(),	ega_xhair(),	ega_rubbox(),
	ega_rubline(),	ega_setxhair(),	ega_setrbox(),	ega_setrline(),
	ega_textmode(),	ega_grafmode(),	ega_vertline(),	ega_circle(),
	ega_fillcirc(),	ega_bittext(),	ega_getrast(),	ega_getcurs(),	
	ega_putcurs(),	ega_version(),	ega_setorig(),	ega_vidwidth(),
	ega_outport(),	ega_setdith(),	ega_dithdraw(),	ega_putdith(),
	ega_dithbox(),	ega_dithline(),	ega_dithcirc(),	ega_dithover(),
	ega_dithpix(),	ega_getpanel(),	ega_putpanel();

void (*ega_funct[39])() = {
	ega_draw,	ega_move,	ega_putrast,	ega_setcolor,
	ega_setpix,	ega_getpix,	ega_horline,	ega_erase,
	ega_putpal,	ega_raster,	ega_xhair,	ega_rubbox,
	ega_rubline,	ega_setxhair,	ega_setrbox,	ega_setrline,
	ega_textmode,	ega_grafmode,	ega_vertline,	ega_circle,
	ega_fillcirc,	ega_bittext,	ega_getrast,	ega_getcurs,	
	ega_putcurs,	ega_version,	ega_setorig,	ega_vidwidth,
	ega_outport,	ega_setdith,	ega_dithdraw,	ega_putdith,
	ega_dithbox,	ega_dithline,	ega_dithcirc,	ega_dithover,
	ega_dithpix,	ega_getpanel,	ega_putpanel
};

static struct ega_struct {
    int arg1;
    int arg2;
    int arg3;
    int arg4;
    int arg5;
    unsigned char *ptr1;
    unsigned char *ptr2;
    unsigned char *ptr3;
    unsigned char *ptr4;
} ega_args;

static int *ega_argp;

/*============ variables global to the driver ========================*/
static struct {
    int x;			/* current x coordinate */
    int y;			/* current y coordinate */
    unsigned char color;	/* current plotting color */
    unsigned char dither;	/* current dither pattern number */
    int offset;			/* current video offset */
} current;

static struct {
    int old_x;			/* old mouse cursor x coordinate */
    int old_y;			/* old mouse cursor y coordinate */
    int fixed_x;		/* fixed x coord. of box or line */
    int fixed_y;		/* fixed y coord. of box or line */
    int float_x;		/* floating x coordinate */
    int float_y;		/* floating y coordinate */
} mouse;

static int bytes_per_row, video_width, video_height;
static int wind_x1, wind_y1, wind_x2, wind_y2;
static int ega_busy, new_byte;
static int start_x, start_y, end_x, end_y;
static unsigned char *graphics_base, *text_base, *save_gbase, *sptalloc();
static unsigned char curs_low, curs_high;
static unsigned char ega_screen[4000];
static unsigned char red_index, grn_index, blu_index;
static unsigned char red_byte, grn_byte, blu_byte;
static unsigned char red_even[80];
static unsigned char grn_even[80];
static unsigned char blu_even[80];
static unsigned char red_odd[80];
static unsigned char grn_odd[80];
static unsigned char blu_odd[80];

static unsigned char bit_val[]    = { 128, 64, 32, 16,  8,  4,  2,  1 }; 
static unsigned char left_mask[]  = { 255,127, 63, 31, 15,  7,  3,  1 };
static unsigned char right_mask[] = { 128,192,224,240,248,252,254,255 };
static unsigned char plane_val[]  = { 1, 2, 4, 8 };

/*==== Array of bit values that are ORed together for color patterns =====

    Bit patterns for bit planes:

	25% pixels set:  00 10 00 10  even lines
			 10 00 10 00  odd lines

	50% pixels set:  01 10 01 10  even lines
			 10 01 10 01  odd lines

	75% pixels set:  11 01 11 01  even lines
			 01 11 01 11  odd lines			      */

static unsigned char dither_bits[2][5] = {
				{ 0,  34, 102, 221, 255 },
				{ 0, 136, 153, 119, 255 } };

/**************** Function: ega_start *************************************
 *
 * This function sets the text mode and prints the driver version.
 */
ega_start()
{
    register int offset, n;

    /* allocate video memory to the driver */
    ega_allocmem();

    /* save the text screen */
    for (offset = 0; offset < 4000; offset++)
     	*(ega_screen + offset) = *(text_base + offset);

    /* set the text mode */
    ega_textmode();

    /* restore the text screen */
    for (offset = 0; offset < 4000; offset++)
     	*(text_base + offset) = *(ega_screen + offset);

    /* print the version */
    printf("\311");
    for (n = 0; n < 44; n++) printf("\315");
    printf("\273\n");
    printf("\272  ");
    printf("   EGA Device Driver     Version %s   ", version);
    printf("  \272\n");
    printf("\272  ");
    printf("Author: Paul W. Carlson     FTS 832-6795");
    printf("  \272\n");
    printf("\310");
    for (n = 0; n < 44; n++) printf("\315");
    printf("\274\n\n");

    /* free video memory */
    ega_freemem();
}



/**************** Function: ega_allocmem *********************************
 *
 * This function allocates the video memory to the driver.
 */
ega_allocmem()
{

    /* allocate graphics video memory to the driver */
    graphics_base = NULL;
    graphics_base = sptalloc(btoc(0x10000), PG_P | PG_LOCK, btoc(0xA0000), 0);
    if (graphics_base == NULL) 
    {	u.u_error = EIO;
	return;
    }
    save_gbase = graphics_base;

    /* allocate text video memory to the driver */
    text_base = NULL;
    text_base = sptalloc(btoc(0x1000), PG_P | PG_LOCK, btoc(0xB8000), 0);
    if (text_base == NULL)
    {	u.u_error = EIO;
	return;
    }
}



/**************** Function: ega_freemem *********************************
 *
 * This function frees the video memory.
 */
ega_freemem()
{

    /* free graphics video memory */
    if (graphics_base != NULL) sptfree(graphics_base, btoc(0x10000), 0);

    /* free text video memory */
    if (text_base != NULL) sptfree(text_base, btoc(0x1000), 0);
}


/**************** Function: ega_open *************************************
 *
 * This function opens the driver and allocates video memory to the driver.
 */
ega_open(dev, flag, otyp)
dev_t dev;
int flag, otyp;
{
    int off;

    /* Check if already open */
    if (ega_busy)
    {	u.u_error = EBUSY;
	return;
    }
    else ega_busy = 1;
    	
    /* allocate video memory to the driver */
    ega_allocmem();

    /* set the logical video dimensions */
    bytes_per_row = 80;
    video_width  = H_RES;
    video_height = V_RES;

    /* set the window */
    wind_x1 = wind_y1 = 0;
    wind_x2 = H_RES - 1;
    wind_y2 = V_RES - 1;
    

    /* clear the saved screen array */
    off = 0;
    while (off < 4000)
    {	*(ega_screen + (off++)) = 32;
	*(ega_screen + (off++)) = 7;
    }
}



/************* Function: ega_ioctl ********************************
 *
 * This function calls all the graphics functions built into the driver.
 */
ega_ioctl(dev, cmd, argp, mode)
dev_t dev;
int cmd, *argp, mode;
{
    int s;

    ega_argp = argp;
    copyin(ega_argp, &ega_args, sizeof(struct ega_struct));
    (*ega_funct[cmd & 63])();
}


/************* Function: ega_close ********************************
 *
 * This function frees the video memory.
 */
ega_close(dev, flag, otyp)
dev_t dev;
int flag, otyp;
{
    /* free video memory */
    ega_freemem();

    /* set flag to not busy */
    ega_busy = 0;
}


/*********** Function: ega_move *************************************
 *
 * This function moves the graphics cursor to ega_args.arg1,ega_args.arg2
 * and computes the video segment number and offset.
 *
 * Elements of structure ega_args:
 *   On entry - 
 *	arg1	= x coordinate
 *	arg2	= y coordinate
 *  On exit -
 *	unchanged
 */
void ega_move()
{
    /* set the current coordinates */
    current.x = ega_args.arg1;
    current.y = ega_args.arg2;
    
    /* compute the video offset */
    current.offset = (bytes_per_row * current.y + 
	(current.x >> 3)) & 0xffff;
}


/************ Function: ega_draw *************************************
 *
 * This function draws a line from current.x,current.y to 
 * ega_args.arg1, ega_args.arg2.
 *
 * Elements of structure ega_args:
 *   On entry - 
 *	arg1	= x coordinate
 *	arg2	= y coordinate
 *  On exit -
 *	unchanged
 */
void ega_draw()
{
    register int x, dx, dy;
    int xinc, yinc, err, errinc, errdec; 
    unsigned char bit_pos;
    
    /* get initial x coord. and bit position */
    x = current.x;
    bit_pos = x & 7;

    /* compute delta x, delta y, and x and y increments */
    dx = ega_args.arg1 - current.x;
    if (dx > 0) 
    {	xinc = 1;
	new_byte = 0;
    }
    else
    {	xinc = -1;
	new_byte = 7;
    	dx = -dx;
    }
    dy = ega_args.arg2 - current.y;
    if (dy > 0) yinc = bytes_per_row;
    else
    {	yinc = -bytes_per_row;
    	dy = -dy;
    }

    /* select the GDC Bit Mask register */
    outb(GDC_IND_SEL, 0x08);
    
    /* if delta x is greater than delta y */
    if (dx > dy)
    {	errinc = dy << 1;
    	errdec = errinc - (dx << 1);
	err = errinc - dx;
	for (dx++; ; dx--)
	{   outb(GDC_IND_REG, bit_val[bit_pos]);
	    *(graphics_base + current.offset) |= 255;
	    if (dx == 1) break;
	    x += xinc;
	    bit_pos = x & 7;
	    if (bit_pos == new_byte) current.offset += xinc;
	    if (err < 0) err += errinc;
	    else
	    {	current.offset += yinc;
	    	err += errdec;
	    }
	}
    }
    
    /* if delta x is less or equal to delta y */
    else
    {	errinc = dx << 1;
    	errdec = errinc - (dy << 1);
	err = errinc - dy;
	for (dy++; ; dy--)
	{   outb(GDC_IND_REG, bit_val[bit_pos]);
	    *(graphics_base + current.offset) |= 255;
	    if (dy == 1) break;
	    current.offset += yinc;
	    if (err < 0) err += errinc;
	    else
	    {	x += xinc;
	   	bit_pos = x & 7;
	    	if (bit_pos == new_byte) current.offset += xinc;
	    	err += errdec;
	    }
	}
    }
    
    /* update current coordinates */
    current.x = ega_args.arg1;
    current.y = ega_args.arg2;
}
    


/*********** Function: ega_putrast *****************************
 *
 * This function fills a rectangular area.
 *
 * Elements of structure ega_args:
 *   On entry - 
 *	arg1	= left x coordinate
 *	arg2	= top y coordinate
 *	arg3	= right x coordinate
 *	arg4	= bottom y coordinate
 *	ptr1	= address of user array filled with bit pattern
 *  On exit -
 *	unchanged
 */
void ega_putrast()
{
    register int y;
    int x1, x2, offset, num_bytes, delta;

    /* compute number of bytes per line */
    x1 = ega_args.arg1;
    x2 = ega_args.arg3;
    num_bytes = (x2 >> 3) - (x1 >> 3) + 1;

    /* compute the video offset */
    offset = (bytes_per_row * ega_args.arg2 + (x1 >> 3)) & 0xffff;

    /* compute the delta offset (end-to-start) */
    delta = bytes_per_row - num_bytes + 1;

    /* select the GDC Bit Mask register */
    outb(GDC_IND_SEL, 0x08);
    
    for (y = ega_args.arg2; y <= ega_args.arg4; y++, offset += delta)
    {
	/* if just one byte per line */
	if (num_bytes == 1)
	{
	    /* set the bit mask and plot the pixels */
	    outb(GDC_IND_REG, (left_mask[x1 & 7] & right_mask[x2 & 7]));
	    *(graphics_base + offset) |= 255;
	}
	else
	{
	    /* do the left byte */
	    outb(GDC_IND_REG, left_mask[x1 & 7]);
	    *(graphics_base + offset) |= 255;
	    offset++;

	    /* do the middle bytes, if any */
	    if (num_bytes > 2)
	    {
		/* set the bit mask */
		outb(GDC_IND_REG, 0xff);

		/* plot the pixels */
		copyin(ega_args.ptr1, graphics_base + offset, num_bytes - 2);
		offset += num_bytes - 2;
	    }

	    /* do the right byte */
	    outb(GDC_IND_REG, right_mask[x2 & 7]);
	    *(graphics_base + offset) |= 255;
	}
    }
}



/*********** Function: ega_setcolor **************************
 *
 * This function stores ega_args.arg1 in current.color.
 *
 * Elements of structure ega_args:
 *   On entry - 
 *	arg1	= color number ( 0 - 15 )
 *  On exit -
 *	unchanged
 */
void ega_setcolor()
{
    /* Write Mode 0 using Set/Reset */
    Set_Reset_Mode();

    /* set the current color number */
    current.color = (unsigned char)ega_args.arg1;

    /* put color number in Set/Reset register */
    outb(GDC_IND_SEL, 0x00);
    outb(GDC_IND_REG, current.color);
}



/************* Function: ega_setpix **************************
 *
 * This function sets a pixel at ega_args.arg1,ega_args.arg2 in 
 * the current.color.
 *
 * Elements of structure ega_args:
 *   On entry - 
 *	arg1	= x coordinate
 *	arg2	= y coordinate
 *  On exit -
 *	unchanged
 */
void ega_setpix()
{
    int offset;

    /* compute the video offset */
    offset = (bytes_per_row * ega_args.arg2 + 
	(ega_args.arg1 >> 3)) & 0xffff;

    /* set the bit mask */
    outb(GDC_IND_SEL, 0x08);
    outb(GDC_IND_REG, bit_val[ega_args.arg1 & 7]);
    
    /* set the pixel */
    *(graphics_base + offset) |= 255;
}


/*********** Function: ega_getpix ***************************
 *
 * This function reads the color number of the pixel at 
 * ega_args.arg1,ega_args.arg2.
 *
 * Elements of structure ega_args:
 *   On entry - 
 *	arg1	= x coordinate
 *	arg2	= y coordinate
 *  On exit -
 *	arg3	= pixel color number
 */
void ega_getpix()
{
    unsigned char plane;
    int offset, color, bit;

    /* compute the video offset and bit value */
    offset = (bytes_per_row * ega_args.arg2 + 
	(ega_args.arg1 >> 3)) & 0xffff;
    bit = bit_val[ega_args.arg1 & 7];

    /* select GDC Map Select register */
    outb(GDC_IND_SEL, 0x04);

    /* clear color */
    color = 0;

    /* loop thru the planes, testing the bit in each plane */
    for (plane = 0; plane < 4; plane++)
    {	outb(GDC_IND_REG, plane);
	if ((*(graphics_base + offset)) & bit) 
	    color |= plane_val[plane];
    }
    
    /* return the pixel color */
    ega_args.arg3 = color;
    copyout(&ega_args, ega_argp, sizeof(struct ega_struct));
}
    

/************ Function: ega_horline ******************************
 *
 * This function draws a horizontal line (much fastrer than move()
 * and draw().
 *
 * Elements of structure ega_args:
 *   On entry - 
 *	arg1	= starting x coordinate
 *	arg2	= ending x coordinate
 *	arg3	= y coordinate
 *	ptr1   	= pointer to user array filled with bit pattern
 *  On exit -
 *	unchanged
 */
void ega_horline()
{
    int x1, x2, y, offset, num_bytes;


    /* check the coordinates, then plot the line */
    x1 = ega_args.arg1;
    x2 = ega_args.arg2;
    y = ega_args.arg3;
    if (y >= 0 && y < video_height && x1 <= x2)
    {	if (x1 < 0) x1 = 0;
	if (x2 >= video_width) x2 = video_width - 1;

    	/* compute number of bytes per line */
    	num_bytes = (x2 >> 3) - (x1 >> 3) + 1;

    	/* compute the video offset */
    	offset = (bytes_per_row * y + (x1 >> 3)) & 0xffff;

    	/* select the GDC Bit Mask register */
    	outb(GDC_IND_SEL, 0x08);
    
	/* if just one byte per line */
	if (num_bytes == 1)
	{
	    /* set the bit mask and plot the pixels */
	    outb(GDC_IND_REG, (left_mask[x1 & 7] & right_mask[x2 & 7]));
	    *(graphics_base + offset) |= 255;
	}
	else
	{
	    /* do the left byte */
	    outb(GDC_IND_REG, left_mask[x1 & 7]);
	    *(graphics_base + offset) |= 255;
	    offset++;

	    /* do the middle bytes, if any */
	    if (num_bytes > 2)
	    {
		/* set the bit mask */
		outb(GDC_IND_REG, 0xff);

		/* plot the pixels */
		copyin(ega_args.ptr1, graphics_base + offset, num_bytes - 2);
		offset += num_bytes - 2;
	    }

	    /* do the right byte */
	    outb(GDC_IND_REG, right_mask[x2 & 7]);
	    *(graphics_base + offset) |= 255;
	}

	/* update current coordinates */
	current.x = x2;
	current.y = y;
    }
}


/************ Function: ega_erase ******************************
 *
 * This function clears the video buffer.
 *
 * Elements of structure ega_args:
 *   On entry - 
 *	ptr1   	= pointer to user array containing bit pattern
 *  On exit -
 *	unchanged
 */
void ega_erase()
{
    int offset;

    /* set the bit mask */
    outb(GDC_IND_SEL, 0x08);
    outb(GDC_IND_REG, 0xff);

    /* set the color */
    outb(GDC_IND_SEL, 0x00);
    outb(GDC_IND_REG, 0x00);

    /* plot the pixels */
    for (offset = 0; offset < 0x10000; offset += 0x0400)
     	copyin(ega_args.ptr1, graphics_base + offset, 0x0400);
}


/*********** Function: ega_putpal ***************************
 *
 * This function sets the palette position's color bits.
 *
 * Elements of structure ega_args:
 *   On entry - 
 *	arg1	= palette position
 *	arg2	= color bits (R'G'B'RGB)
 *  On exit -
 *	unchanged
 */
void ega_putpal()
{
    /* wait for start of vertical retrace */
    while (!(inb(INP_STAT_1) & 8));
    while (inb(INP_STAT_1) & 8);

    /* now change the color bits */
    ATC_index_mode();
    outb(ATC_IND_SEL, (unsigned char)ega_args.arg1 & 0x0f);
    outb(ATC_IND_REG, (unsigned char)ega_args.arg2 & 0x3f);
    outb(ATC_IND_SEL, 0x20);
}


/*********** Function: ega_getpal ***************************
 *
 * This function reads the color bits (R'G'B'RGB) for the palette position.
 * NOTE: This function will not work on a standard EGA card since the
 * ATC registers are read only.
 *
 * Elements of structure ega_args:
 *   On entry - 
 *	arg1	= palette position
 *  On exit -
 *	arg2	= color bits (R'G'B'RGB)
 *
void ega_getpal()
{
    ** wait for start of vertical retrace **
    while (!(inb(INP_STAT_1) & 8));
    while (inb(INP_STAT_1) & 8);

    ** now get the color bits **
    ATC_index_mode();
    outb(ATC_IND_SEL, (unsigned char)ega_args.arg1 & 0x0f);
    ega_args.arg2 = inb(ATC_IND_REG) & 0x3f;
    outb(ATC_IND_SEL, 0x20);
    copyout(&ega_args, ega_argp, sizeof(struct ega_struct));
} */

/************ Function: ega_raster ******************************
 *
 * This function displays a horizontal line of pixels.
 *
 * Elements of structure ega_args:
 *   On entry - 
 *	arg1	= y coordinate
 *	ptr1   	= pointer to user array for plane 0
 *	ptr2   	= pointer to user array for plane 1
 *	ptr3   	= pointer to user array for plane 2
 *	ptr4   	= pointer to user array for plane 3
 *  On exit -
 *	unchanged
 */
void ega_raster()
{
    int offset;

    /* compute the video offset */
    offset = (bytes_per_row * ega_args.arg1) & 0xffff;

    /* set the bit mask */
    outb(GDC_IND_SEL, 0x08);
    outb(GDC_IND_REG, 0xff);

    /* disable Set/Reset for all planes */
    outb(GDC_IND_SEL, 0x01);
    outb(GDC_IND_REG, 0x00);

    /* select the Sequencer Map Mask register */
    outb(TS_IND_SEL, 0x02);

    /* write plane 0 */
    outb(TS_IND_REG, 0x01);
    copyin(ega_args.ptr1, graphics_base + offset, bytes_per_row);

    /* write plane 1 */
    outb(TS_IND_REG, 0x02);
    copyin(ega_args.ptr2, graphics_base + offset, bytes_per_row);

    /* write plane 2 */
    outb(TS_IND_REG, 0x04);
    copyin(ega_args.ptr3, graphics_base + offset, bytes_per_row);

    /* write plane 3 */
    outb(TS_IND_REG, 0x08);
    copyin(ega_args.ptr4, graphics_base + offset, bytes_per_row);

    /* enable Set/Reset for all planes */
    outb(GDC_IND_SEL, 0x01);
    outb(GDC_IND_REG, 0x0f);
}

/************ Function: ega_setxhair ******************************
 *
 * This function sets the initial position of the cross-hairs and
 * saves the pixels under them.
 *
 * Elements of structure ega_args:
 *   On entry - 
 *	arg3	= x coordinate
 *	arg4	= y coordinate
 *  On exit -
 *	unchanged
 */
void ega_setxhair()
{
    /* set mouse coordinates and compute endpoints */
    mouse.old_x = ega_args.arg3;
    mouse.old_y = ega_args.arg4;
    end_x = mouse.old_x + 12;
    start_x = end_x - 25;
    if (start_x < wind_x1) start_x = wind_x1;
    if (end_x   > wind_x2) end_x   = wind_x2;
    end_y = mouse.old_y + 12;
    start_y = end_y - 25;
    if (start_y < wind_y1) start_y = wind_y1;
    if (end_y   > wind_y2) end_y   = wind_y2;
    
    /* enable Set/Reset with XOR function */
    outb(GDC_IND_SEL, 0x01); 
    outb(GDC_IND_REG, 0x0F);
    outb( TS_IND_SEL, 0x02);
    outb( TS_IND_REG, 0x0F);
    outb(GDC_IND_SEL, 0x03);
    outb(GDC_IND_REG, 0x18);
    outb(GDC_IND_SEL, 0x00);
    outb(GDC_IND_REG, 0x0f);

    /* plot the  horizontal cross-hair */
    ega_args.arg1 = start_x;
    ega_args.arg2 = mouse.old_y;
    ega_move();
    ega_args.arg1 = end_x;
    ega_draw();

    /* plot the vertical cross-hair */
    ega_args.arg1 = mouse.old_x;
    ega_args.arg2 = start_y;
    ega_move();
    ega_args.arg2 = end_y;
    ega_draw();
    
    /* set replace function */
    outb(GDC_IND_SEL, 0x03);
    outb(GDC_IND_REG, 0x00);
}



/***************** Function: ega_xhair *****************************
 *
 * This function erases the cross-hair at the previous location and, if
 * ega_args.arg5 > 0, moves the cross_hair to ega_args.arg3,ega_args.arg4.
 *
 * Elements of structure ega_args:
 *   On entry - 
 *	arg3	= floating x coordinate
 *	arg4	= floating y coordinate
 *	arg5	= flag (0 = just erase, > 0 = erase and replot)
 *  On exit -
 *	unchanged
 */
void ega_xhair()
{
    
    /* save new endpoints */
    mouse.float_x = ega_args.arg3;
    mouse.float_y = ega_args.arg4;
    
    /* set XOR function */
    outb(GDC_IND_SEL, 0x03);
    outb(GDC_IND_REG, 0x18);

    /* erase horizontal cross-hair */
    ega_args.arg1 = start_x;
    ega_args.arg2 = mouse.old_y;
    ega_move();
    ega_args.arg1 = end_x;
    ega_draw();
    
    /* erase vertical cross-hair */
    ega_args.arg1 = mouse.old_x;
    ega_args.arg2 = start_y;
    ega_move();
    ega_args.arg2 = end_y;
    ega_draw();

    if (ega_args.arg5)
    {	
	/* compute new endpoints */
	mouse.old_x = mouse.float_x;
    	mouse.old_y = mouse.float_y;
    	end_x = mouse.float_x + 12;
    	start_x = end_x - 25;
    	if (start_x < wind_x1) start_x = wind_x1;
    	if (end_x   > wind_x2) end_x   = wind_x2;
    	end_y = mouse.float_y + 12;
    	start_y = end_y - 25;
    	if (start_y < wind_y1) start_y = wind_y1;
    	if (end_y   > wind_y2) end_y   = wind_y2;
    
    	/* plot the new horizontal cross-hair */
    	ega_args.arg1 = start_x;
    	ega_args.arg2 = mouse.old_y;
    	ega_move();
    	ega_args.arg1 = end_x;
    	ega_draw();
    
    	/* plot the new vertical cross-hair */
    	ega_args.arg1 = mouse.old_x;
    	ega_args.arg2 = start_y;
    	ega_move();
    	ega_args.arg2 = end_y;
    	ega_draw();
    }
    
    /* set replace function */
    outb(GDC_IND_SEL, 0x03);
    outb(GDC_IND_REG, 0x00);
}


/*************** Function: ega_setrbox ****************************
 *
 * This function sets the initial position of the rubber box and
 * saves the pixels under it.
 *
 * Elements of structure ega_args:
 *   On entry - 
 *	arg1	= fixed x coordinate
 *	arg2	= fixed y coordinate
 *	arg3	= floating x coordinate
 *	arg4	= floating y coordinate
 *  On exit -
 *	unchanged
 */
void ega_setrbox()
{
    /* set mouse coordinates and compute endpoints */
    if (ega_args.arg3 >= ega_args.arg1)
    {	start_x = ega_args.arg1;
    	end_x = ega_args.arg3;
    }
    else
    {	start_x = ega_args.arg3;
    	end_x = ega_args.arg1;
    }
    if (ega_args.arg4 >= ega_args.arg2)
    {	start_y = ega_args.arg2;
    	end_y = ega_args.arg4;
    }
    else
    {	start_y = ega_args.arg4;
    	end_y = ega_args.arg2;
    }
    mouse.fixed_x = ega_args.arg1;
    mouse.fixed_y = ega_args.arg2;
    mouse.float_x = ega_args.arg3;
    mouse.float_y = ega_args.arg4;
    
    /* enable Set/Reset with XOR function */
    outb(GDC_IND_SEL, 0x01); 
    outb(GDC_IND_REG, 0x0F);
    outb( TS_IND_SEL, 0x02);
    outb( TS_IND_REG, 0x0F);
    outb(GDC_IND_SEL, 0x03);
    outb(GDC_IND_REG, 0x18);
    outb(GDC_IND_SEL, 0x00);
    outb(GDC_IND_REG, 0x0f);
    
    /* plot the box */
    ega_args.arg1 = start_x;
    ega_args.arg2 = start_y;
    ega_move();
    ega_args.arg1 = end_x;
    ega_draw();
    ega_args.arg2 = end_y;
    ega_draw();
    ega_args.arg1 = start_x;
    ega_draw();
    ega_args.arg2 = start_y;
    ega_draw();
    
    /* set replace function */
    outb(GDC_IND_SEL, 0x03);
    outb(GDC_IND_REG, 0x00);
}


/**************** Function: ega_rubbox ***************************
 *
 * This function erases the rubber box at the previous location and,
 * if ega_args.arg5 > 0, moves the floating corner to 
 * ega_args.arg3,ega_args.arg4.
 *
 * Elements of structure ega_args:
 *   On entry - 
 *	arg3	= floating x coordinate
 *	arg4	= floating y coordinate
 *	arg5	= flag (0 = just erase, > 0 = erase and replot)
 *  On exit -
 *	unchanged
 */
void ega_rubbox()
{

    /* save the coordinates */
    mouse.float_x = ega_args.arg3;
    mouse.float_y = ega_args.arg4;
    
    /* set XOR function */
    outb(GDC_IND_SEL, 0x03);
    outb(GDC_IND_REG, 0x18);
    
    /* erase the box */
    ega_args.arg1 = start_x;
    ega_args.arg2 = start_y;
    ega_move();
    ega_args.arg1 = end_x;
    ega_draw();
    ega_args.arg2 = end_y;
    ega_draw();
    ega_args.arg1 = start_x;
    ega_draw();
    ega_args.arg2 = start_y;
    ega_draw();
    
    if (ega_args.arg5)
    {
    	/* compute new endpoints */
    	mouse.float_x = ega_args.arg3;
    	mouse.float_y = ega_args.arg4;
    	if (mouse.float_x < mouse.fixed_x) 
    	{   start_x = mouse.float_x;
    	    end_x = mouse.fixed_x;
    	}
    	else 
    	{   start_x = mouse.fixed_x;
    	    end_x = mouse.float_x;
    	}
    	if (mouse.float_y < mouse.fixed_y) 
    	{   start_y = mouse.float_y;
    	    end_y = mouse.fixed_y;
    	}
    	else 
    	{   start_y = mouse.fixed_y;
    	    end_y = mouse.float_y;
    	}

    	/* plot the new box */
    	ega_args.arg1 = start_x;
    	ega_args.arg2 = start_y;
    	ega_move();
    	ega_args.arg1 = end_x;
    	ega_draw();
    	ega_args.arg2 = end_y;
    	ega_draw();
    	ega_args.arg1 = start_x;
    	ega_draw();
    	ega_args.arg2 = start_y;
    	ega_draw();
    }
    
    /* set replace function */
    outb(GDC_IND_SEL, 0x03);
    outb(GDC_IND_REG, 0x00);
}


/*********** Function: ega_setrline ****************************
 *
 * This function sets the initial position of the rubber line and
 * saves the pixels under it.
 *
 * Elements of structure ega_args:
 *   On entry - 
 *	arg1	= fixed x coordinate
 *	arg2	= fixed y coordinate
 *	arg3	= floating x coordinate
 *	arg4	= floating y coordinate
 *  On exit -
 *	unchanged
 */
void ega_setrline()
{
    /* get endpoints */
    start_x = ega_args.arg1;
    start_y = ega_args.arg2;
    end_x = ega_args.arg3;
    end_y = ega_args.arg4;
    
    /* enable Set/Reset with XOR function */
    outb(GDC_IND_SEL, 0x01); 
    outb(GDC_IND_REG, 0x0F);
    outb( TS_IND_SEL, 0x02);
    outb( TS_IND_REG, 0x0F);
    outb(GDC_IND_SEL, 0x03);
    outb(GDC_IND_REG, 0x18);
    outb(GDC_IND_SEL, 0x00);
    outb(GDC_IND_REG, 0x0f);
    
    /* plot the line */
    ega_move();
    ega_args.arg1 = end_x;
    ega_args.arg2 = end_y;
    ega_draw();
    
    /* set replace function */
    outb(GDC_IND_SEL, 0x03);
    outb(GDC_IND_REG, 0x00);
}


/***************** Function: ega_rubline **************************
 *
 * This function erases the rubber line at the previous location and, if
 * ega_args.arg5 > 0, moves the floating end to ega_args.arg3,ega_args.arg4.
 *
 * Elements of structure ega_args:
 *   On entry - 
 *	arg3	= floating x coordinate
 *	arg4	= floating y coordinate
 *	arg5	= flag (0 = just erase, > 0 = erase and replot)
 *  On exit -
 *	unchanged
 */
void ega_rubline()
{
    /* save new endpoints */
    mouse.float_x = ega_args.arg3;
    mouse.float_y = ega_args.arg4;
    
    /* set XOR function */
    outb(GDC_IND_SEL, 0x03);
    outb(GDC_IND_REG, 0x18);

    /* erase the line */
    ega_args.arg1 = start_x;
    ega_args.arg2 = start_y;
    ega_move();
    ega_args.arg1 = end_x;
    ega_args.arg2 = end_y;
    ega_draw();
    
    if (ega_args.arg5)
    {
    	/* get new endpoints */
    	end_x = mouse.float_x;
    	end_y = mouse.float_y;

    	/* plot the new line */
    	ega_args.arg1 = start_x;
    	ega_args.arg2 = start_y;
    	ega_move();
    	ega_args.arg1 = end_x;
    	ega_args.arg2 = end_y;
    	ega_draw();
    }
    
    /* set replace function */
    outb(GDC_IND_SEL, 0x03);
    outb(GDC_IND_REG, 0x00);
}



/***************** Function: ega_textmode **************************
 *
 * This function puts the Orchid card in 80 column color text mode.
 */
void ega_textmode()
{
    register int position, offset, ram_off;
    register int char_no, char_row;
    unsigned char index;
    int i;

    /*==== set registers, load font, then set registers again ====*/
    for (i = 0; i < 2; i++)
    {

        /* enable color by setting bit 0 of Misc. Output Register */
        outb(WR_MISC_OUT, inb(RD_MISC_OUT) | 1);

        /* halt the timing sequencer and select BIOS ROM Address Map 1 */
        unprotect();
        outb(TS_IND_SEL, 0x00);
        outb(TS_IND_REG, 0x01);
        outb(TS_IND_SEL, 0x07);
        outb(TS_IND_REG, 0x08);

        /* set clock and polarity for mode 0x03 */
        outb(WR_MISC_OUT, 0xa3);

        /* set CRTC register 0x24 (Compatibility Control) */
        outb(HERC_COMP, 0x03);
        outb(MODE_CONTROL, 0xa0);
        outb(CRTC_IND_SEL, 0x24);
        outb(CRTC_IND_REG, 0x00);

        /* set the TS registers */
        for (index = 0x01; index < 0x05; index++)
        {   outb(TS_IND_SEL, index);
	    outb(TS_IND_REG, A_TSvals[index]);
        }
        outb(MODE_CONTROL, 0x29);
        outb(HERC_COMP, 0x01);
        outb(WR_MISC_OUT, 0xa3);
        for (index = 0x06; index < 0x08; index++)
        {   outb(TS_IND_SEL, index);
	    outb(TS_IND_REG, A_TSvals[index]);
        }
        outb(TS_IND_SEL, 0x00);
        outb(TS_IND_REG, 0x03);

        /* set the CRTC registers */
        unprotect();
        for (index = 0x00; index < 0x19; index++)
        {   outb(CRTC_IND_SEL, index);
	    outb(CRTC_IND_REG, A_CRTvals[index]);
        }
        outb(CRTC_IND_SEL, 0x23);
        outb(CRTC_IND_REG, 0x00);
        outb(CRTC_IND_SEL, 0x25);
        outb(CRTC_IND_REG, 0x00);

        /* set the ATC registers */
        ATC_index_mode();
        for (index = 0x00; index < 0x17; index++)
    	{   while (!(inb(INP_STAT_1) & 8));
    	    while (inb(INP_STAT_1) & 8);
            outb(ATC_IND_SEL, index);
	    outb(ATC_IND_REG, A_ATCvals[index]);
        }

        /* set the GDC registers */
        for (index = 0x00; index < 0x09; index++)
        {   outb(GDC_IND_SEL, index);
	    outb(GDC_IND_REG, A_GDCvals[index]);
        }
        outb(GDC_IND_SEL, 0x0d);
        outb(GDC_IND_REG, 0x00);
        outb(GDC_SEG_SEL, 0x00);

        /* if first time thru loop, load the font from ROM into RAM */
	if (!i)
	{

	    /* disable the ATC */
	    outb(ATC_IND_SEL, 0x00);

	    /* write enable bit plane 2 */
	    outb(TS_IND_SEL, 0x02);
	    outb(TS_IND_REG, 0x04);

	    /* enable font selection, sequential access */
	    outb(TS_IND_SEL, 0x04);
	    outb(TS_IND_REG, 0x07);

	    /* write mode 0 */
	    outb(GDC_IND_SEL, 0x05);
	    outb(GDC_IND_REG, 0x00);

	    /* memory map 0xA0000 - 0xB0000 */
	    outb(GDC_IND_SEL, 0x06);
	    outb(GDC_IND_REG, 0x04);
	
	    /* load the 8x14 chatacter map into RAM */
	    ram_off = 0;
	    offset = 0;
	    graphics_base = save_gbase;
	    for (char_no = 0; char_no < 256; char_no++)
	    {   for (char_row = 0; char_row < 14; char_row++)
	    	{   *(graphics_base + (ram_off++)) = *(char_map + (offset++));
	        }
	        ram_off += 18;
    	    }
        }
    }

    /* restore the text screen */
    for (offset = 0; offset < 4000; offset++)
     	*(text_base + offset) = *(ega_screen + offset);

    /* enable the ATC */
    outb(ATC_IND_SEL, 0x20);
}



/***************** Function: ega_grafmode **************************
 *
 * This function sets the 640x350 16 color mode.
 */
void ega_grafmode()
{
    register int text_offset;
    register unsigned char *offset;
    unsigned char index, position;

    /* save the text screen */
    for (text_offset = 0; text_offset < 4000; text_offset++)
     	*(ega_screen + text_offset) = *(text_base + text_offset);

    /* enable color by setting bit 0 of Misc. Output Register */
    outb(WR_MISC_OUT, inb(RD_MISC_OUT) | 1);

    /* halt the timing sequencer and select BIOS ROM Address Map 1 */
    unprotect();
    outb(TS_IND_SEL, 0x00);
    outb(TS_IND_REG, 0x01);
    outb(TS_IND_SEL, 0x07);
    outb(TS_IND_REG, 0x08);

    /* set clock and polarity for mode 0x10 */
    outb(WR_MISC_OUT, 0xa3);

    /* set CRTC register 0x24 (Compatibility Control) */
    outb(HERC_COMP, 0x03);
    outb(MODE_CONTROL, 0xa0);
    outb(CRTC_IND_SEL, 0x24);
    outb(CRTC_IND_REG, 0x00);

    /* set the TS registers */
    for (index = 0x01; index < 0x05; index++)
    {	outb(TS_IND_SEL, index);
	outb(TS_IND_REG, G_TSvals[index]);
    }
    outb(MODE_CONTROL, 0x29);
    outb(HERC_COMP, 0x01);
    outb(WR_MISC_OUT, 0xa3);
    for (index = 0x06; index < 0x08; index++)
    {	outb(TS_IND_SEL, index);
	outb(TS_IND_REG, G_TSvals[index]);
    }
    outb(TS_IND_SEL, 0x00);
    outb(TS_IND_REG, 0x02);

    /* set the CRTC registers */
    unprotect();
    for (index = 0x00; index < 0x19; index++)
    {	outb(CRTC_IND_SEL, index);
	outb(CRTC_IND_REG, G_CRTvals[index]);
    }
    outb(CRTC_IND_SEL, 0x23);
    outb(CRTC_IND_REG, 0x00);
    outb(CRTC_IND_SEL, 0x25);
    outb(CRTC_IND_REG, 0x00);

    /* set the ATC registers */
    ATC_index_mode();
    for (index = 0x00; index < 0x17; index++)
    {	outb(ATC_IND_SEL, index);
	outb(ATC_IND_REG, G_ATCvals[index]);
    }

    /* set the GDC registers */
    for (index = 0x00; index < 0x09; index++)
    {	outb(GDC_IND_SEL, index);
	outb(GDC_IND_REG, G_GDCvals[index]);
    }
    outb(GDC_IND_SEL, 0x0d);
    outb(GDC_IND_REG, 0x00);
    outb(GDC_SEG_SEL, 0x00);

    /* set registers to use Set/Rest */
    Set_Reset_Mode();

    /* clear video memory */
    outb(GDC_IND_SEL, 0x08);
    outb(GDC_IND_REG, 0xff);
    outb(GDC_IND_SEL, 0x00);
    outb(GDC_IND_REG, 0x00);
    graphics_base = save_gbase;
    for (offset = graphics_base; offset < graphics_base + 0x10000; offset++)
		*offset = 0;

    /* enable the ATC */
    outb(ATC_IND_SEL, 0x20);
}


static ATC_index_mode()
{
    /* set the ATC index/data flip-flop to index
     * mode by reading the Input Status 1 register.
     */
    inb(INP_STAT_1);
}


static unprotect()
{
    unsigned char CRTC11;

    /* disable protection on TS register 7 and CRTC
     * registers 0 - 7 by setting bit 7 of CRTC
     * register 0x11 to 0.  Vertical interrupts are
     * disabled by setting bit 5 to 1.
     */
    outb(CRTC_IND_SEL, 0x11);
    CRTC11 = inb(CRTC_IND_REG);
    outb(CRTC_IND_SEL, 0x11);
    outb(CRTC_IND_REG, (CRTC11 & 0x7f) | 0x20);
}



/************ Function: ega_vertline *****************************
 *
 * This function draws a vertical line.
 *
 * Elements of structure ega_args:
 *   On entry - 
 *	arg1	= starting y coordinate
 *	arg2	= ending y coordinate
 *	arg3	= x coordinate
 *  On exit -
 *	unchanged
 */
void ega_vertline()
{
    register int y;
    
    /* compute intial video offset */
    current.offset = (bytes_per_row * ega_args.arg1 + 
	(ega_args.arg3 >>3)) & 0xffff;

    /* set the bit mask */
    outb(GDC_IND_SEL, 0x08);
    outb(GDC_IND_REG, bit_val[ega_args.arg3 & 7]);

    /* plot the pixels */
    for (y = ega_args.arg1; y <= ega_args.arg2; y++, 
	current.offset += bytes_per_row)
	*(graphics_base + current.offset) |= 255;

    /* update the current y coordinate and video offset */
    current.y = ega_args.arg2;
    current.offset -= bytes_per_row;
}



/**************** Function: ega_circle ****************************
 *
 * This function plots a circle.
 *
 * Elements of structure ega_args:
 *   On entry - 
 *	arg1	= center x coordinate
 *	arg2	= center y coordinate
 *	arg3	= radius
 *  On exit -
 *	unchanged
 */
void ega_circle()
{
    int x, y, cx, cy, r;
    long int dx1000, dy1000, prevdx, prevdy, savdx, savdy;
    long int radius, dx, dy;
    
    cx = ega_args.arg1;
    cy = ega_args.arg2;
    r  = ega_args.arg3;
    radius = (long)r;
    dx = prevdx = radius;
    dx1000 = 1000 * dx;
    for ( dy = 0; dy < dx; dy++)
    {   dx1000 -= (dy * 1881) / dx;
        dx = (dx1000 + 500) / 1000;
	savdx = dx;
	y = (int)dy;
	do
	{   x = (int)dx;
	    ega_args.arg1 = cx + x;
	    ega_args.arg2 = cy - y;
	    ega_setpix();
	    ega_args.arg1 = cx - x;
	    ega_setpix();
	    ega_args.arg2 = cy + y;
	    ega_setpix();
	    ega_args.arg1 = cx + x;
	    ega_setpix();
	} while (++dx < prevdx);
	dx = savdx;
	prevdx = dx;
    }
    prevdy = dy;
    dy1000 = 1000 * dy;
    dx--;
    do
    {	dy = (dy1000 + 500) / 1000;
	savdy = dy;
	x = (int)dx;
	do
	{   y = (int)dy;
	    ega_args.arg1 = cx + x;
	    ega_args.arg2 = cy - y;
	    ega_setpix();
	    ega_args.arg1 = cx - x;
	    ega_setpix();
	    ega_args.arg2 = cy + y;
	    ega_setpix();
	    ega_args.arg1 = cx + x;
	    ega_setpix();
	} while (--dy > prevdy);
	dy = savdy;
	prevdy = dy;
	dy1000 += ((dx - 1) * 532) / (long)y;
    } while (dx-- >= 0);		
}



/*************** Function: ega_fillcirc ****************************
 *
 * This function plots a filled circle.
 *
 * Elements of structure ega_args:
 *   On entry - 
 *	arg1	= center x coordinate
 *	arg2	= center y coordinate
 *	arg3	= radius
 *  On exit -
 *	unchanged
 */
void ega_fillcirc()
{
    int x, y, cx, cy, r;
    long int dx1000, dy1000, prevdx, prevdy, savdx, savdy;
    long int radius, dx, dy;
    
    cx = ega_args.arg1;
    cy = ega_args.arg2;
    r  = ega_args.arg3;
    radius = (long)r;
    dx = prevdx = radius;
    dx1000 = 1000 * dx;
    for ( dy = 0; dy < dx; dy++)
    {   dx1000 -= (dy * 1881) / dx;
        dx = (dx1000 + 500) / 1000;
	savdx = dx;
	y = (int)dy;
	do
	{   x = (int)dx;
	    ega_args.arg1 = cx - x;
	    ega_args.arg2 = cx + x;
	    ega_args.arg3 = cy + y;
	    ega_horline();
	    ega_args.arg3 = cy - y;
	    ega_horline();
	} while (++dx < prevdx);
	dx = savdx;
	prevdx = dx;
    }
    prevdy = dy;
    dy1000 = 1000 * dy;
    dx--;
    do
    {	dy = (dy1000 + 500) / 1000;
	savdy = dy;
	x = (int)dx;
	do
	{   y = (int)dy;
	    ega_args.arg1 = cx - x;
	    ega_args.arg2 = cx + x;
	    ega_args.arg3 = cy + y;
	    ega_horline();
	    ega_args.arg3 = cy - y;
	    ega_horline();
	} while (--dy > prevdy);
	dy = savdy;
	prevdy = dy;
	dy1000 += ((dx - 1) * 532) / (long)y;
    } while (dx-- >= 0);		
}


/*************** Function: ega_bittext ****************************
 *
 * This function plots bitmapped text starting at cordinates
 * ega_args.arg1,ega_args.arg2.  The characters are 8x14 pixels.
 *
 * Elements of structure ega_args:
 *   On entry - 
 *	arg1	= starting x coordinate
 *	arg2	= starting y coordinate
 *	arg3	= length of text
 *	ptr1	= pointer to text
 *  On exit -
 *	unchanged
 */
void ega_bittext() 
{ 
    int pat_off, char_num, col, row, x, y, leng, offset, x_off;
    unsigned char text[128], *ptr, bit; 

    Set_Reset_Mode();

    x = ega_args.arg1;
    y = ega_args.arg2;
    leng = ega_args.arg3;
    copyin(ega_args.ptr1, text, leng);
 
    y -= 13;
    outb(GDC_IND_SEL, 0x08);
    for (char_num = 0; char_num < leng; char_num++) 
    {   pat_off = text[char_num] * 14; 
        ptr = char_map + pat_off; 
        for (col = 0; col < 8; col++) 
	{   x_off = x + col;
	    bit = bit_val[x_off & 7];
            for (row = 0; row < 14; row++) 
                if ((*(ptr+row)) & (bit_val[col])) 
    	    	{   offset = (bytes_per_row * (y + row) + 
			(x_off >> 3)) & 0xffff;
		    outb(GDC_IND_REG, bit);
    	    	    *(graphics_base + offset) |= 255;
		}
        } 
        x += 8; 
    } 
} 


/************ Function: ega_getrast ******************************
 *
 * This function gets a horizontal line of pixels.
 *
 * Elements of structure ega_args:
 *   On entry - 
 *	arg1	= y coordinate
 *  On exit -
 *	ptr1   	= pointer to user array for plane 0
 *	ptr2   	= pointer to user array for plane 1
 *	ptr3   	= pointer to user array for plane 2
 *	ptr4   	= pointer to user array for plane 3
 */
void ega_getrast()
{
    int offset;

    /* compute the video offset */
    offset = (bytes_per_row * ega_args.arg1) & 0xffff;

    /* select the GDC Map Select register */
    outb(GDC_IND_SEL, 0x04);

    /* read plane 0 */
    outb(GDC_IND_REG, 0x00);
    copyout(graphics_base + offset, ega_args.ptr1, bytes_per_row);

    /* read plane 1 */
    outb(GDC_IND_REG, 0x01);
    copyout(graphics_base + offset, ega_args.ptr2, bytes_per_row);

    /* read plane 2 */
    outb(GDC_IND_REG, 0x02);
    copyout(graphics_base + offset, ega_args.ptr3, bytes_per_row);

    /* read plane 3 */
    outb(GDC_IND_REG, 0x03);
    copyout(graphics_base + offset, ega_args.ptr4, bytes_per_row);
}



/************ Function: ega_getcurs ******************************
 *
 * This function saves the CRTC cursor address registers.
 */
void ega_getcurs()
{
	
    outb(CRTC_IND_SEL, 0x0e);
    curs_high = inb(CRTC_IND_REG);
    outb(CRTC_IND_SEL, 0x0f);
    curs_low = inb(CRTC_IND_REG);
}



/************ Function: ega_putcurs ******************************
 *
 * This function restores the CRTC cursor address registers.
 */
void ega_putcurs()
{
	
    outb(CRTC_IND_SEL, 0x0e);
    outb(CRTC_IND_REG, curs_high);
    outb(CRTC_IND_SEL, 0x0f);
    outb(CRTC_IND_REG, curs_low);
}


/************ Function: ega_version ******************************
 *
 * This function gets the current version number.
 */
void ega_version()
{
    int n;

    for (n = 0; version[n]; n++);
    copyout((unsigned char *)version, ega_args.ptr1, n);
}


/************ Function: ega_setorig ******************************
 *
 * This function sets the origin of the display.
 *
 * Elements of structure ega_args:
 *   On entry - 
 *	arg1	= x coordinate
 *	arg2	= y coordinate
 *  On exit -
 *	unchanged
 */
void ega_setorig()
{
    int offset;

    /* set the new window */
    wind_x1 = ega_args.arg1;
    wind_y1 = ega_args.arg2;
    wind_x2 = wind_x1 + H_RES - 1;
    wind_y2 = wind_y1 + V_RES - 1;

    /* set the new graphics start address */
    offset = (wind_y1 * bytes_per_row + (wind_x1 >> 3)) & 0xffff;
    graphics_base = save_gbase + offset;

    /* wait for start of display interval */
    while (inb(INP_STAT_1) & 8);
    while (!(inb(INP_STAT_1) & 8));

    /* set the display start address */
    outb(CRTC_IND_SEL, 0x0c);
    outb(CRTC_IND_REG, (offset & 0xff00) >> 8);
    outb(CRTC_IND_SEL, 0x0d);
    outb(CRTC_IND_REG, offset & 0x00ff);

    /* wait for start of vertical retrace */
    while (!(inb(INP_STAT_1) & 8));
    while (inb(INP_STAT_1) & 8);

    /* set the horizontal PEL panning register */
    inb(INP_STAT_1);
    outb(ATC_IND_SEL, 0x13);
    outb(ATC_IND_REG, wind_x1 & 7);
    outb(ATC_IND_SEL, 0x20);
}


/************ Function: ega_vidwidth ******************************
 *
 * This function sets the logical width of the video buffer.
 *
 * Elements of structure ega_args:
 *   On entry - 
 *	arg1	= width in bytes
 *  On exit -
 *	unchanged
 */
void ega_vidwidth()
{
    unsigned char words;

    /* compute number of words and logical display width */
    words = (unsigned char)((ega_args.arg1 / 2));
    if (words > 93) words = 93;
    bytes_per_row = (int)(words + words);
    video_width = 8 * bytes_per_row;

    /* compute logical display height */
    video_height = 0x10000 / bytes_per_row;

    /* allocate display memory per row */
    outb(CRTC_IND_SEL, 0x13);
    outb(CRTC_IND_REG, words);
}


/************ Function: ega_outport ******************************
 *
 * This function outputs a byte to a port.
 *
 * Elements of structure ega_args:
 *   On entry - 
 *	arg1	= port address
 *	arg2	= byte to output
 *  On exit -
 *	unchanged
 */
void ega_outport()
{
    outb(ega_args.arg1, (unsigned char)(ega_args.arg2 & 0xff));
}


/*============== DITHERED FUNCTIONS ================================*/

/*********** Function: ega_setdith **************************
 *
 * This function stores ega_args.arg1 in current.dither.
 *
 * Elements of structure ega_args:
 *   On entry - 
 *	arg1	= color number ( 0 - 215 )
 *  On exit -
 *	unchanged
 */
void ega_setdith()
{
    register i;
    unsigned char R_even, R_odd, G_even, G_odd, B_even, B_odd;

    /* set the current color number */
    current.dither = (unsigned char)ega_args.arg1;

    /* compute the RGB indices */
    blu_index = current.dither % 5;
    red_index = current.dither / 25;
    grn_index = (current.dither - blu_index - 25 * red_index) / 5;

    /* fill the arrays */
    R_even = dither_bits[0][red_index];
    R_odd  = dither_bits[1][red_index];
    G_even = dither_bits[0][grn_index];
    G_odd  = dither_bits[1][grn_index];
    B_even = dither_bits[0][blu_index];
    B_odd  = dither_bits[1][blu_index];
    for (i = 0; i < 80; i++)
    {	red_even[i] = R_even;
    	grn_even[i] = G_even;
    	blu_even[i] = B_even;
    	red_odd[i] = R_odd;
    	grn_odd[i] = G_odd;
    	blu_odd[i] = B_odd;
    }
}


/************ Function: ega_dithdraw *************************************
 *
 * This function draws a line from current.x,current.y to 
 * ega_args.arg1, ega_args.arg2.
 *
 * Elements of structure ega_args:
 *   On entry - 
 *	arg1	= x coordinate
 *	arg2	= y coordinate
 *  On exit -
 *	unchanged
 */
void ega_dithdraw()
{
    register int dx, dy;
    int x, y, xinc, yinc, err, errinc, errdec, pix_color, yline, yi; 
    unsigned char bit, bit_pos;
    
    Set_Reset_Mode();

    /* compute initial values */
    x = current.x;
    y = current.y;
    bit_pos = x & 7;
    bit = bit_val[bit_pos];
    yline = y & 1;

    /* compute delta x, delta y, and x and y increments */
    dx = ega_args.arg1 - current.x;
    if (dx > 0) 
    {	xinc = 1;
	new_byte = 0;
    }
    else
    {	xinc = -1;
	new_byte = 7;
    	dx = -dx;
    }
    dy = ega_args.arg2 - current.y;
    if (dy > 0) 
    {	yinc = bytes_per_row;
	yi = 1;
    }
    else
    {	yinc = -bytes_per_row;
	yi = -1;
    	dy = -dy;
    }

    /* if delta x is greater than delta y */
    if (dx > dy)
    {	errinc = dy << 1;
    	errdec = errinc - (dx << 1);
	err = errinc - dx;
	for (dx++; ; dx--)
	{   pix_color = 0;
	    if (dither_bits[yline][blu_index] & bit) pix_color |= 1;
	    if (dither_bits[yline][grn_index] & bit) pix_color |= 2;
	    if (dither_bits[yline][red_index] & bit) pix_color |= 4;
	    outb(GDC_IND_SEL, 0x00);
	    outb(GDC_IND_REG, pix_color);
	    outb(GDC_IND_SEL, 0x08);
	    outb(GDC_IND_REG, bit);
	    *(graphics_base + current.offset) |= 255;
	    if (dx == 1) break;
	    x += xinc;
	    bit_pos = x & 7;
	    bit = bit_val[bit_pos];
	    if (bit_pos == new_byte) current.offset += xinc;
	    if (err < 0) err += errinc;
	    else
	    {	current.offset += yinc;
		y += yi;
		yline = y & 1;
	    	err += errdec;
	    }
	}
    }
    
    /* if delta x is less or equal to delta y */
    else
    {	errinc = dx << 1;
    	errdec = errinc - (dy << 1);
	err = errinc - dy;
	for (dy++; ; dy--)
	{   pix_color = 0;
	    if (dither_bits[yline][blu_index] & bit) pix_color |= 1;
	    if (dither_bits[yline][grn_index] & bit) pix_color |= 2;
	    if (dither_bits[yline][red_index] & bit) pix_color |= 4;
	    outb(GDC_IND_SEL, 0x00);
	    outb(GDC_IND_REG, pix_color);
	    outb(GDC_IND_SEL, 0x08);
	    outb(GDC_IND_REG, bit);
	    *(graphics_base + current.offset) |= 255;
	    if (dy == 1) break;
	    current.offset += yinc;
	    y += yi;
	    yline = y & 1;
	    if (err < 0) err += errinc;
	    else
	    {	x += xinc;
	   	bit_pos = x & 7;
	    	bit = bit_val[bit_pos];
	    	if (bit_pos == new_byte) current.offset += xinc;
	    	err += errdec;
	    }
	}
    }
    /* update current coordinates */
    current.x = ega_args.arg1;
    current.y = ega_args.arg2;
}


/*********** Function: ega_putdith *****************************
 *
 * This function fills a rectangular area.
 *
 * Elements of structure ega_args:
 *   On entry - 
 *	arg1	= left x coordinate
 *	arg2	= top y coordinate
 *	arg3	= right x coordinate
 *	arg4	= bottom y coordinate
 *	ptr1	= address of user array filled with color numbers
 *  On exit -
 *	unchanged
 */
void ega_putdith()
{
    register unsigned char *ptr;
    register int x, y;
    int n, xpos, ypos, offset, num_bytes;
    unsigned char lmask, rmask, lclear, rclear;
    unsigned char red_start, red_end, grn_start, grn_end; 
    unsigned char blu_start, blu_end, bit;
    unsigned char *R_even, *R_odd, *G_even, *G_odd, *B_even, *B_odd;
    unsigned char *R_ptr, *G_ptr, *B_ptr, *savptr;

    /* compute the number of bytes and initial video offset */
    num_bytes = (ega_args.arg3 >> 3) - (ega_args.arg1 >> 3) + 1;
    if (num_bytes < 1) return;
    offset = (80 * ega_args.arg2 + (ega_args.arg1 >> 3)) & 0xffff;

    /* set bit mask */
    outb(GDC_IND_SEL, 0x08);	
    outb(GDC_IND_REG, 0xFF);

    /* disable Set/Reset */
    outb(GDC_IND_SEL, 0x01);	
    outb(GDC_IND_REG, 0x00);

    /* select Read Map Select register */
    outb(GDC_IND_SEL, 0x04);	

    /* select Sequencer Map Mask register */
    outb( TS_IND_SEL, 0x02);	

    /* compute the masks */
    lmask = left_mask[ega_args.arg1 & 7]; 
    rmask = right_mask[ega_args.arg3 & 7];
    lclear = ~lmask;
    rclear = ~rmask;

    /* compute the middle bytes, if any, for even and odd lines */
    if (num_bytes > 2)
    {	ptr = ega_args.ptr1 - (ega_args.arg1 & 7) + 8;
	R_even = red_even;
	G_even = grn_even;
	B_even = blu_even;
	R_odd = red_odd;
	G_odd = grn_odd;
	B_odd = blu_odd;
	for (n = 0; n < num_bytes - 2; n++)
        {   *R_even = 0;
            *G_even = 0;
            *B_even = 0;
            *R_odd  = 0;
            *G_odd  = 0;
            *B_odd  = 0;
	    for (x = 0; x < 8; x++)
	    {   current.dither = *ptr++;
    		blu_index = current.dither % 5;
    		red_index = current.dither / 25;
    		grn_index = (current.dither - blu_index - 25 * red_index) / 5;
	    	bit = bit_val[x & 7];
            	*R_even |= (dither_bits[0][red_index] & bit);
            	*G_even |= (dither_bits[0][grn_index] & bit);
            	*B_even |= (dither_bits[0][blu_index] & bit);
            	*R_odd  |= (dither_bits[1][red_index] & bit);
            	*G_odd  |= (dither_bits[1][grn_index] & bit);
            	*B_odd  |= (dither_bits[1][blu_index] & bit);
	    }
            R_even++;
            G_even++;
            B_even++;
            R_odd++;
            G_odd++;
            B_odd++;
	}
    }
    savptr = ptr;

    /* for each row ... */
    for (y = ega_args.arg2; y <= ega_args.arg4; y++)
    {	ypos = y & 1;
	ptr = ega_args.ptr1;

        /* get first and last bytes from video memory */
        outb(GDC_IND_REG, 0x00);
        blu_start = *(graphics_base + offset);
        blu_end = *(graphics_base + offset + num_bytes - 1);
        outb(GDC_IND_REG, 0x01);
        grn_start = *(graphics_base + offset);
        grn_end = *(graphics_base + offset + num_bytes - 1);
        outb(GDC_IND_REG, 0x02);
        red_start = *(graphics_base + offset);
        red_end = *(graphics_base + offset + num_bytes - 1);

	/* if just one byte per line */
	if (num_bytes == 1)
	{
	    /* clear the bytes */
	    blu_start &= (lclear | rclear);
	    grn_start &= (lclear | rclear);
	    red_start &= (lclear | rclear);
    
	    /* OR in bits from color patterns */
            for (x = ega_args.arg1; x <= ega_args.arg3; x++)
	    {   current.dither = *ptr++;
    		blu_index = current.dither % 5;
    		red_index = current.dither / 25;
    		grn_index = (current.dither - blu_index - 25 * red_index) / 5;
	    	bit = bit_val[x & 7];
            	red_start |= (dither_bits[ypos][red_index] & bit);
            	grn_start |= (dither_bits[ypos][grn_index] & bit);
            	blu_start |= (dither_bits[ypos][blu_index] & bit);
	    }

	    /* copy the bytes to video memory */
            outb(TS_IND_REG, 0x01);
	    bcopy(&blu_start, graphics_base + offset, 1);
            outb(TS_IND_REG, 0x02);
	    bcopy(&grn_start, graphics_base + offset, 1);
            outb(TS_IND_REG, 0x04);
	    bcopy(&red_start, graphics_base + offset, 1);
	}
	else
	{
	    /* do the left byte */

	    /* clear the bytes */
	    blu_start &= lclear;
	    grn_start &= lclear;
	    red_start &= lclear;
    
	    /* OR in bits from color patterns */
            x = ega_args.arg1;
	    do
	    {   xpos = x & 7;
		current.dither = *ptr++;
    		blu_index = current.dither % 5;
    		red_index = current.dither / 25;
    		grn_index = (current.dither - blu_index - 25 * red_index) / 5;
	    	bit = bit_val[xpos];
            	red_start |= (dither_bits[ypos][red_index] & bit);
            	grn_start |= (dither_bits[ypos][grn_index] & bit);
            	blu_start |= (dither_bits[ypos][blu_index] & bit);
		x++;
	    } while (xpos != 7);


	    /* copy the bytes to video memory */
            outb(TS_IND_REG, 0x01);
	    bcopy(&blu_start, graphics_base + offset, 1);
            outb(TS_IND_REG, 0x02);
	    bcopy(&grn_start, graphics_base + offset, 1);
            outb(TS_IND_REG, 0x04);
	    bcopy(&red_start, graphics_base + offset, 1);

	    /* do the middle bytes, if any */
	    if (num_bytes > 2)
	    {	if (ypos)
		{   R_ptr = red_odd;
		    G_ptr = grn_odd;
		    B_ptr = blu_odd;
		}
		else
		{   R_ptr = red_even;
		    G_ptr = grn_even;
		    B_ptr = blu_even;
		}

		/* copy the arrays to video memory */
        	outb(TS_IND_REG, 0x01);
		bcopy(B_ptr, graphics_base + offset + 1, num_bytes - 2);
        	outb(TS_IND_REG, 0x02);
		bcopy(G_ptr, graphics_base + offset + 1, num_bytes - 2);
        	outb(TS_IND_REG, 0x04);
		bcopy(R_ptr, graphics_base + offset + 1, num_bytes - 2);
	    }
	    else 
	    {
		savptr = ega_args.ptr1 + x - ega_args.arg1;
	    }

	    /* do the right byte */

	    /* clear the bytes */
	    blu_end &= rclear;
	    grn_end &= rclear;
	    red_end &= rclear;

	    /* OR in bits from color patterns */
	    ptr = savptr;
	    for (x = 0; x < 8 && (x & 7) <= (ega_args.arg3 & 7); x++)
	    {   current.dither = *ptr++;
    		blu_index = current.dither % 5;
    		red_index = current.dither / 25;
    		grn_index = (current.dither - blu_index - 25 * red_index) / 5;
	    	bit = bit_val[x & 7];
            	red_end |= (dither_bits[ypos][red_index] & bit);
            	grn_end |= (dither_bits[ypos][grn_index] & bit);
            	blu_end |= (dither_bits[ypos][blu_index] & bit);
	    }

	    /* copy the bytes to video memory */
            outb(TS_IND_REG, 0x01);
	    bcopy(&blu_end, graphics_base + offset + num_bytes - 1, 1);
            outb(TS_IND_REG, 0x02);
	    bcopy(&grn_end, graphics_base + offset + num_bytes - 1, 1);
            outb(TS_IND_REG, 0x04);
	    bcopy(&red_end, graphics_base + offset + num_bytes - 1, 1);
	}
	offset += 80;
    }
}


/*********** Function: ega_dithbox *****************************
 *
 * This function fills a rectangular area with the current dither.
 *
 * Elements of structure ega_args:
 *   On entry - 
 *	arg1	= left x coordinate
 *	arg2	= top y coordinate
 *	arg3	= right x coordinate
 *	arg4	= bottom y coordinate
 *  On exit -
 *	unchanged
 */
void ega_dithbox()
{
    register int y;
    int ypos, offset; 
    int num_bytes;
    unsigned char *video_ptr, *ptr, lmask, rmask, lclear, rclear;


    /* compute the number of bytes and initial video offset */
    num_bytes = (ega_args.arg3 >> 3) - (ega_args.arg1 >> 3) + 1;
    if (num_bytes < 1) return;
    offset = (80 * ega_args.arg2 + (ega_args.arg1 >> 3)) & 0xffff;

    /* set bit mask */
    outb(GDC_IND_SEL, 0x08);	
    outb(GDC_IND_REG, 0xFF);

    /* disable Set/Reset */
    outb(GDC_IND_SEL, 0x01);	
    outb(GDC_IND_REG, 0x00);

    /* select Read Map Select register */
    outb(GDC_IND_SEL, 0x04);	

    /* select Sequencer Map Mask register */
    outb( TS_IND_SEL, 0x02);	

    /* compute the masks */
    lmask = left_mask[ega_args.arg1 & 7]; 
    rmask = right_mask[ega_args.arg3 & 7];
    lclear = ~lmask;
    rclear = ~rmask;

    /* for each row ... */
    for (y = ega_args.arg2; y <= ega_args.arg4; y++)
    {	ypos = y & 1;
    	video_ptr = graphics_base + offset;

	/* get the left byte from each bitplane */
        outb(GDC_IND_REG, 0x00);
        blu_byte = *video_ptr;
        outb(GDC_IND_REG, 0x01);
        grn_byte = *video_ptr;
        outb(GDC_IND_REG, 0x02);
        red_byte = *video_ptr;

	/* if just one byte per line */
	if (num_bytes == 1)
	{

	    /* mask the bytes */
	    blu_byte &= (lclear | rclear);
	    grn_byte &= (lclear | rclear);
	    red_byte &= (lclear | rclear);
    
	    /* OR in bits from color patterns */
	    blu_byte |= (dither_bits[ypos][blu_index] & lmask & rmask);
	    grn_byte |= (dither_bits[ypos][grn_index] & lmask & rmask);
	    red_byte |= (dither_bits[ypos][red_index] & lmask & rmask);

	    /* copy bytes to video memory */
    	    outb( TS_IND_REG, 0x01);
	    bcopy(&blu_byte, video_ptr, 1);
    	    outb( TS_IND_REG, 0x02);
	    bcopy(&grn_byte, video_ptr, 1);
    	    outb( TS_IND_REG, 0x04);
	    bcopy(&red_byte, video_ptr, 1);
	}
	else
	{
	    /* do the left bytes */

	    /* mask the bytes */
	    blu_byte &= lclear;
	    grn_byte &= lclear;
	    red_byte &= lclear;
    
	    /* OR in bits from color patterns */
	    blu_byte |= (dither_bits[ypos][blu_index] & lmask);
	    grn_byte |= (dither_bits[ypos][grn_index] & lmask);
	    red_byte |= (dither_bits[ypos][red_index] & lmask);

	    /* copy bytes to video memory */
    	    outb( TS_IND_REG, 0x01);
	    bcopy(&blu_byte, video_ptr, 1);
    	    outb( TS_IND_REG, 0x02);
	    bcopy(&grn_byte, video_ptr, 1);
    	    outb( TS_IND_REG, 0x04);
	    bcopy(&red_byte, video_ptr, 1);
    	    video_ptr++;

	    /* copy the middle bytes, if any */
	    if (num_bytes > 2)
	    {	outb( TS_IND_REG, 0x01);
		ptr = (ypos) ? blu_odd : blu_even;
	    	bcopy(ptr, video_ptr, num_bytes - 2);
    	    	outb( TS_IND_REG, 0x02);
		ptr = (ypos) ? grn_odd : grn_even;
	    	bcopy(ptr, video_ptr, num_bytes - 2);
    	    	outb( TS_IND_REG, 0x04);
		ptr = (ypos) ? red_odd : red_even;
	    	bcopy(ptr, video_ptr, num_bytes - 2);
    	    	video_ptr += num_bytes - 2;
	    }

	    /* do the right bytes */

	    /* get the right byte from each bitplane */
            outb(GDC_IND_REG, 0x00);
            blu_byte = *video_ptr;
            outb(GDC_IND_REG, 0x01);
            grn_byte = *video_ptr;
            outb(GDC_IND_REG, 0x02);
            red_byte = *video_ptr;

	    /* mask the bytes */
	    blu_byte &= rclear;
	    grn_byte &= rclear;
	    red_byte &= rclear;
    
	    /* OR in bits from color patterns */
	    blu_byte |= (dither_bits[ypos][blu_index] & rmask);
	    grn_byte |= (dither_bits[ypos][grn_index] & rmask);
	    red_byte |= (dither_bits[ypos][red_index] & rmask);

	    /* copy bytes to video memory */
    	    outb( TS_IND_REG, 0x01);
	    bcopy(&blu_byte, video_ptr, 1);
    	    outb( TS_IND_REG, 0x02);
	    bcopy(&grn_byte, video_ptr, 1);
    	    outb( TS_IND_REG, 0x04);
	    bcopy(&red_byte, video_ptr, 1);
	}
	offset += 80;
    }
}


/*********** Function: ega_dithline *****************************
 *
 * This function fills a horizontal line with the current dither.
 *
 * Elements of structure ega_args:
 *   On entry - 
 *	arg1	= left x coordinate
 *	arg2	= right x coordinate
 *	arg3	= y coordinate
 *  On exit -
 *	unchanged
 */
void ega_dithline()
{
    int ypos, offset; 
    int num_bytes;
    unsigned char *video_ptr, *ptr, lmask, rmask, lclear, rclear;


    /* compute the number of bytes and initial video offset */
    num_bytes = (ega_args.arg2 >> 3) - (ega_args.arg1 >> 3) + 1;
    if (num_bytes < 1) return;
    offset = (80 * ega_args.arg3 + (ega_args.arg1 >> 3)) & 0xffff;
    ypos = ega_args.arg3 & 1;
    video_ptr = graphics_base + offset;

    /* set bit mask */
    outb(GDC_IND_SEL, 0x08);	
    outb(GDC_IND_REG, 0xFF);

    /* disable Set/Reset */
    outb(GDC_IND_SEL, 0x01);	
    outb(GDC_IND_REG, 0x00);

    /* select Read Map Select register */
    outb(GDC_IND_SEL, 0x04);	

    /* select Sequencer Map Mask register */
    outb( TS_IND_SEL, 0x02);	

    /* compute the masks */
    lmask = left_mask[ega_args.arg1 & 7]; 
    rmask = right_mask[ega_args.arg2 & 7];
    lclear = ~lmask;
    rclear = ~rmask;

    /* get the left byte from each bitplane */
    outb(GDC_IND_REG, 0x00);
    blu_byte = *video_ptr;
    outb(GDC_IND_REG, 0x01);
    grn_byte = *video_ptr;
    outb(GDC_IND_REG, 0x02);
    red_byte = *video_ptr;

    /* if just one byte per line */
    if (num_bytes == 1)
    {

	/* mask the bytes */
	blu_byte &= (lclear | rclear);
	grn_byte &= (lclear | rclear);
	red_byte &= (lclear | rclear);
    
	/* OR in bits from color patterns */
	blu_byte |= (dither_bits[ypos][blu_index] & lmask & rmask);
	grn_byte |= (dither_bits[ypos][grn_index] & lmask & rmask);
	red_byte |= (dither_bits[ypos][red_index] & lmask & rmask);

	/* copy bytes to video memory */
    	outb( TS_IND_REG, 0x01);
	bcopy(&blu_byte, video_ptr, 1);
    	outb( TS_IND_REG, 0x02);
	bcopy(&grn_byte, video_ptr, 1);
    	outb( TS_IND_REG, 0x04);
	bcopy(&red_byte, video_ptr, 1);
    }
    else
    {
	/* do the left bytes */

	/* mask the bytes */
	blu_byte &= lclear;
	grn_byte &= lclear;
	red_byte &= lclear;
    
	/* OR in bits from color patterns */
	blu_byte |= (dither_bits[ypos][blu_index] & lmask);
	grn_byte |= (dither_bits[ypos][grn_index] & lmask);
	red_byte |= (dither_bits[ypos][red_index] & lmask);

	/* copy bytes to video memory */
    	outb( TS_IND_REG, 0x01);
	bcopy(&blu_byte, video_ptr, 1);
    	outb( TS_IND_REG, 0x02);
	bcopy(&grn_byte, video_ptr, 1);
    	outb( TS_IND_REG, 0x04);
	bcopy(&red_byte, video_ptr, 1);
    	video_ptr++;

	/* copy the middle bytes, if any */
	if (num_bytes > 2)
	{   outb( TS_IND_REG, 0x01);
	    ptr = (ypos) ? blu_odd : blu_even;
	    bcopy(ptr, video_ptr, num_bytes - 2);
    	    outb( TS_IND_REG, 0x02);
	    ptr = (ypos) ? grn_odd : grn_even;
	    bcopy(ptr, video_ptr, num_bytes - 2);
 	    outb( TS_IND_REG, 0x04);
	    ptr = (ypos) ? red_odd : red_even;
	    bcopy(ptr, video_ptr, num_bytes - 2);
    	    video_ptr += num_bytes - 2;
	}

	/* do the right bytes */

	/* get the right byte from each bitplane */
        outb(GDC_IND_REG, 0x00);
        blu_byte = *video_ptr;
        outb(GDC_IND_REG, 0x01);
        grn_byte = *video_ptr;
        outb(GDC_IND_REG, 0x02);
        red_byte = *video_ptr;

	/* mask the bytes */
	blu_byte &= rclear;
	grn_byte &= rclear;
	red_byte &= rclear;
    
	/* OR in bits from color patterns */
	blu_byte |= (dither_bits[ypos][blu_index] & rmask);
	grn_byte |= (dither_bits[ypos][grn_index] & rmask);
	red_byte |= (dither_bits[ypos][red_index] & rmask);

	/* copy bytes to video memory */
    	outb( TS_IND_REG, 0x01);
	bcopy(&blu_byte, video_ptr, 1);
    	outb( TS_IND_REG, 0x02);
        bcopy(&grn_byte, video_ptr, 1);
    	outb( TS_IND_REG, 0x04);
    	bcopy(&red_byte, video_ptr, 1);
    }
}


/*************** Function: ega_dithcirc ****************************
 *
 * This function plots a filled circle with the current dither pattern.
 *
 * Elements of structure ega_args:
 *   On entry - 
 *	arg1	= center x coordinate
 *	arg2	= center y coordinate
 *	arg3	= radius
 *  On exit -
 *	unchanged
 */
void ega_dithcirc()
{
    int x, y, cx, cy, r;
    long int dx1000, dy1000, prevdx, prevdy, savdx, savdy;
    long int radius, dx, dy;
    
    cx = ega_args.arg1;
    cy = ega_args.arg2;
    r  = ega_args.arg3;
    radius = (long)r;
    dx = prevdx = radius;
    dx1000 = 1000 * dx;
    for ( dy = 0; dy < dx; dy++)
    {   dx1000 -= (dy * 1881) / dx;
        dx = (dx1000 + 500) / 1000;
	savdx = dx;
	y = (int)dy;
	do
	{   x = (int)dx;
	    ega_args.arg1 = cx - x;
	    ega_args.arg2 = cx + x;
	    ega_args.arg3 = cy + y;
	    ega_dithline();
	    ega_args.arg3 = cy - y;
	    ega_dithline();
	} while (++dx < prevdx);
	dx = savdx;
	prevdx = dx;
    }
    prevdy = dy;
    dy1000 = 1000 * dy;
    dx--;
    do
    {	dy = (dy1000 + 500) / 1000;
	savdy = dy;
	x = (int)dx;
	do
	{   y = (int)dy;
	    ega_args.arg1 = cx - x;
	    ega_args.arg2 = cx + x;
	    ega_args.arg3 = cy + y;
	    ega_dithline();
	    ega_args.arg3 = cy - y;
	    ega_dithline();
	} while (--dy > prevdy);
	dy = savdy;
	prevdy = dy;
	dy1000 += ((dx - 1) * 532) / (long)y;
    } while (dx-- >= 0);		
}


/*********** Function: ega_dithover *****************************
 *
 * This function overlays a line on the display.
 *
 * Elements of structure ega_args:
 *   On entry - 
 *	arg1	= left x coordinate
 *	arg2	= right x coordinate
 *	arg3	= y coordinate
 *	ptr1	= address of user array filled with color numbers
 *  On exit -
 *	unchanged
 */
void ega_dithover()
{
    register unsigned char pix_color, *ptr;
    register int x;
    int xpos, ypos, offset; 
    int num_bytes;
    unsigned char *video_ptr, bit;

    Set_Reset_Mode();

    /* compute the number of bytes and initial video offset */
    num_bytes = (ega_args.arg2 >> 3) - (ega_args.arg1 >> 3) + 1;
    if (num_bytes < 1) return;
    offset = (80 * ega_args.arg3 + (ega_args.arg1 >> 3)) & 0xffff;
    ypos = ega_args.arg3 & 1;
    video_ptr = graphics_base + offset;

    /* loop thru line... */
    ptr = ega_args.ptr1;
    for (x = ega_args.arg1; x <= ega_args.arg2; x++)
    {	xpos = x & 7;
	current.dither = *ptr++;
	if (current.dither != 255)
	{   pix_color = 0;
	    bit = bit_val[xpos];
    	    blu_index = current.dither % 5;
    	    red_index = current.dither / 25;
    	    grn_index = (current.dither - blu_index - 25 * red_index) / 5;
	    if (dither_bits[ypos][blu_index] & bit) pix_color |= 1;
	    if (dither_bits[ypos][grn_index] & bit) pix_color |= 2;
	    if (dither_bits[ypos][red_index] & bit) pix_color |= 4;
	    outb(GDC_IND_SEL, 0x00);
	    outb(GDC_IND_REG, pix_color);
	    outb(GDC_IND_SEL, 0x08);
	    outb(GDC_IND_REG, bit);
	    *video_ptr |= 255;
	}
	if (xpos == 7) video_ptr++;
    }
}


/*********** Function: ega_dithpix *****************************
 *
 * This function displays a dithered pixel.
 *
 * Elements of structure ega_args:
 *   On entry - 
 *	arg1	= x coordinate
 *	arg2	= y coordinate
 *  On exit -
 *	unchanged
 */
void ega_dithpix()
{
    register unsigned char pix_color;
    register int x;
    int ypos, offset; 
    unsigned char bit;

    Set_Reset_Mode();

    offset = (80 * ega_args.arg3 + (ega_args.arg1 >> 3)) & 0xffff;
    ypos = ega_args.arg3 & 1;
    pix_color = 0;
    bit = bit_val[ega_args.arg1 & 7];
    blu_index = current.dither % 5;
    red_index = current.dither / 25;
    grn_index = (current.dither - blu_index - 25 * red_index) / 5;
    if (dither_bits[ypos][blu_index] & bit) pix_color |= 1;
    if (dither_bits[ypos][grn_index] & bit) pix_color |= 2;
    if (dither_bits[ypos][red_index] & bit) pix_color |= 4;
    outb(GDC_IND_SEL, 0x00);
    outb(GDC_IND_REG, pix_color);
    outb(GDC_IND_SEL, 0x08);
    outb(GDC_IND_REG, bit);
    *(graphics_base + offset) |= 255;
}


/************ Function: ega_getpanel ******************************
 *
 * This function gets a horizontal line of pixels for a panel.
 *
 * Elements of structure ega_args:
 *   On entry - 
 *	arg1	= starting x coordinate
 *	arg2	= ending x coordinate
 *	arg3	= y coordinate
 *  On exit -
 *	ptr1   	= pointer to user array for plane 0
 *	ptr2   	= pointer to user array for plane 1
 *	ptr3   	= pointer to user array for plane 2
 */
void ega_getpanel()
{
    int offset, num_bytes;

    /* compute the number of bytes and initial offset */
    num_bytes = (ega_args.arg2 >> 3) - (ega_args.arg1 >> 3) + 1;
    if (num_bytes < 1) return;
    offset = (80 * ega_args.arg3 + (ega_args.arg1 >> 3)) & 0xffff;

    /* set the bit mask */
    outb(GDC_IND_SEL, 0x08);
    outb(GDC_IND_REG, 0xFF);

    /* select the GDC Map Select register */
    outb(GDC_IND_SEL, 0x04);

    /* read plane 0 */
    outb(GDC_IND_REG, 0x00);
    copyout(graphics_base + offset, ega_args.ptr1, num_bytes);

    /* read plane 1 */
    outb(GDC_IND_REG, 0x01);
    copyout(graphics_base + offset, ega_args.ptr2, num_bytes);

    /* read plane 2 */
    outb(GDC_IND_REG, 0x02);
    copyout(graphics_base + offset, ega_args.ptr3, num_bytes);
}



/************ Function: ega_putpanel ******************************
 *
 * This function displays a horizontal of a panel.
 *
 * Elements of structure ega_args:
 *   On entry - 
 *	arg1	= starting x coordinate
 *	arg2	= ending x coordinate
 *	arg3	= y coordinate
 *	ptr1   	= pointer to user array for plane 0
 *	ptr2   	= pointer to user array for plane 1
 *	ptr3   	= pointer to user array for plane 2
 *  On exit -
 *	unchanged
 */
void ega_putpanel()
{
    int offset, num_bytes;

    /* compute the number of bytes and initial offset */
    num_bytes = (ega_args.arg2 >> 3) - (ega_args.arg1 >> 3) + 1;
    if (num_bytes < 1) return;
    offset = (80 * ega_args.arg3 + (ega_args.arg1 >> 3)) & 0xffff;

    /* set the bit mask */
    outb(GDC_IND_SEL, 0x08);
    outb(GDC_IND_REG, 0xff);

    /* disable Set/Reset for all planes */
    outb(GDC_IND_SEL, 0x01);
    outb(GDC_IND_REG, 0x00);

    /* select the Sequencer Map Mask register */
    outb(TS_IND_SEL, 0x02);

    /* write plane 0 */
    outb(TS_IND_REG, 0x01);
    copyin(ega_args.ptr1, graphics_base + offset, num_bytes);

    /* write plane 1 */
    outb(TS_IND_REG, 0x02);
    copyin(ega_args.ptr2, graphics_base + offset, num_bytes);

    /* write plane 2 */
    outb(TS_IND_REG, 0x04);
    copyin(ega_args.ptr3, graphics_base + offset, num_bytes);
}
