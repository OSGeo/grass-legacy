#include <grass/config.h>

#ifdef HAVE_SOCKET

#include <stdio.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

#include <grass/gis.h>
#include <grass/raster.h>
#include <grass/graphics.h>

#include "transport.h"

/*!
 * \brief screen left edge
 *
 * Returns the coordinate of the left edge of the screen.
 *
 *  \param void
 *  \return int
 */

int REM_screen_left(void)
{
	int l;
	_send_ident(SCREEN_LEFT);
	_get_int(&l);
	return l;
}

/*!
 * \brief screen right edge
 *
 * Returns the coordinate of the right edge of the screen.
 *
 *  \param void
 *  \return int
 */

int REM_screen_rite(void)
{
	int r;
	_send_ident(SCREEN_RITE);
	_get_int(&r);
	return r;
}


/*!
 * \brief bottom of screen
 *
 * Returns the coordinate of the bottom of the screen.
 *
 *  \param void
 *  \return int
 */

int REM_screen_bot(void)
{
	int b;
	_send_ident(SCREEN_BOT);
	_get_int(&b);
	return b;
}


/*!
 * \brief top of screen
 *
 * Returns the coordinate of the top of the screen.
 *
 *  \param void
 *  \return int
 */

int REM_screen_top(void)
{
	int t;
	_send_ident(SCREEN_TOP);
	_get_int(&t);
	return t;
}

int REM_get_num_colors(int *n)
{
	_send_ident(GET_NUM_COLORS);
	_get_int(n);

	return 0;
}

/*!
 * \brief select floating color table
 *
 * Selects a
 * float color table to be used for subsequent color calls. It is expected that
 * the user will follow this call with a call to erase and reinitialize the
 * entire graphics screen.
 * Returns 0 if successful, non-zero if unsuccessful.
 *
 *  \param void
 *  \return int
 */

int REM_color_table_float(void)
{
	int i;
	_send_ident(COLOR_TABLE_FLOAT);
	_get_int(&i);
	return i;
}

/*!
 * \brief select fixed color table
 *
 * Selects a fixed
 * color table to be used for subsequent color calls. It is expected that the
 * user will follow this call with a call to erase and reinitialize the entire
 * graphics screen.
 * Returns 0 if successful, non-zero if unsuccessful.
 *
 *  \param void
 *  \return int
 */

int REM_color_table_fixed(void)
{
	int i;
	_send_ident(COLOR_TABLE_FIXED);
	_get_int(&i);
	return(i);
}

int REM_color_offset(int n)
{
	_send_ident(COLOR_OFFSET);
	_send_int(&n);

	return 0;
}

/*!
 * \brief select color
 *
 * Selects the <b>color</b> to be
 * used in subsequent draw commands.
 *
 *  \param index
 *  \return int
 */

int REM_color(int index)
{
	_send_ident(COLOR);
	_send_int(&index);

	return 0;
}


/*!
 * \brief select standard color
 *
 * Selects the
 * standard <b>color</b> to be used in subsequent draw commands.  The
 * <b>color</b> value is best retrieved using <i>D_translate_color.</i>
 * See Display_Graphics_Library.
 *
 *  \param index
 *  \return int
 */

int REM_standard_color(int index)
{
	_send_ident(STANDARD_COLOR);
	_send_int(&index);

	return 0;
}

/*!
 * \brief select color
 *
 * When in
 * float mode (see <i>R_color_table_float</i>), this call selects the color
 * most closely matched to the <b>red, grn</b>, and <b>blue</b> intensities
 * requested. These values must be in the range of 0-255.
 *
 *  \param red
 *  \param grn
 *  \param blue
 *  \return int
 */

int REM_RGB_color(unsigned char red, unsigned char grn, unsigned char blu)
{
	_send_ident(RGB_COLOR);
	_send_char(&red);
	_send_char(&grn);
	_send_char(&blu);

	return 0;
}

/*!
 * \brief define single color
 *
 * Sets color number <b>num</b> to the
 * intensities represented by <b>red, grn</b>, and <b>blue.</b>
 *
 *  \param red
 *  \param grn
 *  \param blu
 *  \param num number
 *  \return int
 */

int REM_reset_color(unsigned char red, unsigned char grn, unsigned char blu,
		  int index)
{
	if (index < 0)
		index = 256 - index;

	_send_ident(RESET_COLOR);
	_send_char(&red);
	_send_char(&grn);
	_send_char(&blu);
	_send_int(&index);

	return 0;
}

/*!
 * \brief define multiple colors
 *
 * Sets color numbers
 * <b>min</b> through <b>max</b> to the intensities represented in the arrays
 * <b>red, grn, and blue.</b>
 *
 *  \param min
 *  \param max
 *  \param red
 *  \param grn
 *  \param blue
 *  \return int
 */

int REM_reset_colors(int min, int max,
		   unsigned char *red, unsigned char *grn, unsigned char *blu)
{
	/* only send a chunk at a time - to avoid malloc() in the driver */

	while (min <= max)
	{
		int n = max - min + 1;
		int i;

		if (n > 512)
			n = 512;

		_send_ident(RESET_COLORS);
		i = min;
		_send_int(&i);
		i = min + n - 1;
		_send_int(&i);
		_send_char_array(n, red); red += n;
		_send_char_array(n, grn); grn += n;
		_send_char_array(n, blu); blu += n;
		min += n;
	}

	return 0;
}

/*!
 * \brief change the width of line
 *
 * Changes the <b>width</b> of line to be used in subsequent draw commands.
 *
 *  \param width
 *  \return int
 */

int REM_line_width(int width)
{
	_send_ident(LINE_WIDTH);
	_send_int(&width);

	return 0;
}

/*!
 * \brief erase screen
 *
 * Erases the entire screen to black.
 *
 *  \param void
 *  \return int
 */

int REM_erase(void)
{
	_send_ident(ERASE);

	return 0;
}

/*!
 * \brief move current location
 *
 * Move the current location to the absolute screen coordinate <b>x,y.</b>
 * Nothing is drawn on the screen.
 *
 *  \param x
 *  \param y
 *  \return int
 */

int REM_move_abs(int x, int y)
{
	_send_ident(MOVE_ABS);
	_send_int(&x);
	_send_int(&y);

	return 0;
}

/*!
 * \brief move current location
 *
 * Shift the current screen location by the values in <b>dx</b> and <b>dy</b>:
 \code
   Newx = Oldx + dx;
   Newy = Oldy + dy;
 \endcode
 * Nothing is drawn on the screen.
 *
 *  \param x dx
 *  \param y dy
 *  \return int
 */

int REM_move_rel(int x, int y)
{
	_send_ident(MOVE_REL);
	_send_int(&x);
	_send_int(&y);

	return 0;
}

/*!
 * \brief draw line
 *
 * Draw a line using the current color, selected via <i>R_color</i>, from the 
 * current location to the location specified by <b>x,y.</b> The current location
 * is updated to <b>x,y.</b>
 *
 *  \param x
 *  \param y
 *  \return int
 */

int REM_cont_abs(int x, int y)
{
	_send_ident(CONT_ABS);
	_send_int(&x);
	_send_int(&y);

	return 0;
}

/*!
 * \brief draw line
 *
 * Draw a line using the
 * current color, selected via <i>R_color</i>, from the current location to
 * the relative location specified by <b>x</b> and <b>y.</b> The current
 * location is updated:
  \code
   Newx = Oldx + x;
   Newy = Oldy + y;
  \endcode
 *
 *  \param x
 *  \param y
 *  \return int
 */

int REM_cont_rel(int x, int y)
{
	_send_ident(CONT_REL);
	_send_int(&x);
	_send_int(&y);

	return 0;
}

/*!
 * \brief draw a series of dots
 *
 * Pixels at the <b>num</b> absolute positions in the <b>x</b> and
 * <b>y</b> arrays are turned to the current color. The current location is
 * left updated to the position of the last dot.
 *
 *  \param xarray x
 *  \param yarray y
 *  \param number
 *  \return int
 */

int REM_polydots_abs(int *xarray, int *yarray, int number)
{
	_send_ident(POLYDOTS_ABS);
	_send_int(&number);
	_send_int_array(number, xarray);
	_send_int_array(number, yarray);

	return 0;
}

/*!
 * \brief draw a series of dots
 *
 * Pixels at the <b>number</b> relative positions in the <b>x</b> and
 * <b>y</b> arrays are turned to the current color. The first position is
 * relative to the starting current location; the succeeding positions are then
 * relative to the previous position. The current location is updated to the
 * position of the last dot.
 *
 *  \param xarray x
 *  \param yarray y
 *  \param number
 *  \return int
 */

int REM_polydots_rel(int *xarray, int  *yarray, int number)
{
	_send_ident(POLYDOTS_REL);
	_send_int(&number);
	_send_int_array(number, xarray);
	_send_int_array(number, yarray);

	return 0;
}

/*!
 * \brief draw an open polygon
 *
 * The <b>number</b> absolute positions in the <b>x</b> and <b>y</b>
 * arrays are used to generate a multisegment line (often curved). This line is
 * drawn with the current color. The current location is left updated to the
 * position of the last point.
 * <b>Note.</b> It is not assumed that the line is closed, i.e., no line is
 * drawn from the last point to the first point.
 *
 *  \param xarray x
 *  \param yarray y
 *  \param number
 *  \return int
 */

int REM_polyline_abs(int *xarray, int  *yarray, int number)
{
	_send_ident(POLYLINE_ABS);
	_send_int(&number);
	_send_int_array(number, xarray);
	_send_int_array(number, yarray);

	return 0;
}

/*!
 * \brief draw an open polygon
 *
 * The <b>number</b> relative positions in the <b>x</b> and <b>y</b>
 * arrays are used to generate a multisegment line (often curved). The first
 * position is relative to the starting current location; the succeeding
 * positions are then relative to the previous position. The current location is
 * updated to the position of the last point. This line is drawn with the current
 * color.
 * <b>Note.</b> No line is drawn between the last point and the first point.
 *
 *  \param xarray x
 *  \param yarray y
 *  \param number
 *  \return int
 */

int REM_polyline_rel(int *xarray, int *yarray, int number)
{
	_send_ident(POLYLINE_REL);
	_send_int(&number);
	_send_int_array(number, xarray);
	_send_int_array(number, yarray);

	return 0;
}

/*!
 * \brief draw a closed polygon
 *
 * The <b>number</b> absolute positions in the <b>x</b> and <b>y</b> arrays
 * outline a closed polygon which is filled with the current color. The current
 * location is undefined afterwards.
 *
 *  \param xarray x
 *  \param yarray y
 *  \param number
 *  \return int
 */

int REM_polygon_abs(int *xarray, int *yarray, int number)
{
	_send_ident(POLYGON_ABS);
	_send_int(&number);
	_send_int_array(number, xarray);
	_send_int_array(number, yarray);

	return 0;
}

/*!
 * \brief draw a closed polygon
 *
 * The <b>number</b> relative positions in the <b>x</b> and <b>y</b>
 * arrays outline a closed polygon which is filled with the current color. The
 * first position is relative to the starting current location; the succeeding
 * positions are then relative to the previous position. The current location is
 * undefined afterwards.
 *
 *  \param xarray x
 *  \param yarray y
 *  \param number
 *  \return int
 */

int REM_polygon_rel(int *xarray, int *yarray, int number)
{
	_send_ident(POLYGON_REL);
	_send_int(&number);
	_send_int_array(number, xarray);
	_send_int_array(number, yarray);

	return 0;
}

/*!
 * \brief fill a box
 *
 * A box is drawn in the current color using the coordinates <b>x1,y1</b> and
 * <b>x2,y2</b> as opposite corners of the box. The current location is undefined
 * afterwards
 *
 *  \param x1
 *  \param y1
 *  \param x2
 *  \param y2
 *  \return int
 */

int REM_box_abs(int x1, int y1, int x2, int y2)
{
	_send_ident(BOX_ABS);
	_send_int(&x1);
	_send_int(&y1);
	_send_int(&x2);
	_send_int(&y2);

	return 0;
}


/*!
 * \brief fill a box
 *
 * A box is drawn in the current color using the current location as one corner 
 * and the current location plus <b>x</b> and <b>y</b> as the opposite corner 
 * of the box. The current location is undefined afterwards.
 *
 *  \param x
 *  \param y
 *  \return int
 */

int REM_box_rel(int x, int y)
{
	_send_ident(BOX_REL);
	_send_int(&x);
	_send_int(&y);

	return 0;
}

/*!
 * \brief set text size
 *
 * Sets text pixel width and height to <b>width</b> and <b>height.</b>
 *
 *  \param width
 *  \param height
 *  \return int
 */

int REM_text_size(int width, int height)
{
	_send_ident(TEXT_SIZE);
	_send_int(&width);
	_send_int(&height);

	return 0;
}

int REM_text_rotation(float rotation)
{
	_send_ident(TEXT_ROTATION);
	_send_float(&rotation);

	return 0;
}

/*!
 * \brief set text clipping frame
 *
 * Subsequent calls to <i>R_text</i> will have text strings
 * clipped to the screen frame defined by <b>top, bottom, left, right.</b>
 *
 *  \param t top
 *  \param b bottom
 *  \param l left
 *  \param r right
 *  \return int
 */

int REM_set_window(int t, int b, int l, int r)
{
	_send_ident(SET_WINDOW);
	_send_int(&t);
	_send_int(&b);
	_send_int(&l);
	_send_int(&r);

	return 0;
}

/*!
 * \brief write text
 *
 * Writes <b>text</b> in the current color and font, at the current text
 * width and height, starting at the current screen location.
 *
 *  \param sometext
 *  \return int
 */

int REM_text(const char *sometext)
{
	_send_ident(TEXT);
	_send_text(sometext);

	return 0;
}

/*!
 * \brief get text extents
 *
 * The extent of the area enclosing the <b>text</b>
 * is returned in the integer pointers <b>top, bottom, left</b>, and
 * <b>right.</b> No text is actually drawn. This is useful for capturing the
 * text extent so that the text location can be prepared with proper background
 * or border.
 *
 *  \param sometext
 *  \param t top
 *  \param b bottom
 *  \param l left
 *  \param r right
 *  \return int
 */

int REM_get_text_box(const char *sometext, int *t, int *b, int *l, int *r)
{
	_send_ident(GET_TEXT_BOX);
	_send_text(sometext);
	_get_int(t);
	_get_int(b);
	_get_int(l);
	_get_int(r);

	return 0;
}

static int select_font(const char *name)
{
	char filename[4096];
	int stat;

	sprintf(filename, "%s/fonts/%s", G_gisbase(), name);

	_send_ident(FONT);
	_send_text(filename);
	_get_int(&stat);

	return stat == 0;
}

/*!
 * \brief choose font
 *
 * Set current font to <b>font name</b>. Available fonts are:
 * 
 <table>
 <tr><td><b>Font Name</b></td><td><b>Description</b></td></tr>
 <tr><td>cyrilc </td><td> cyrillic</td></tr>
 <tr><td>gothgbt </td><td> Gothic Great Britain triplex</td></tr>
 <tr><td>gothgrt </td><td>  Gothic German triplex</td></tr>
 <tr><td>gothitt </td><td>  Gothic Italian triplex</td></tr>
 <tr><td>greekc </td><td> Greek complex</td></tr>
 <tr><td>greekcs </td><td> Greek complex script</td></tr>
 <tr><td>greekp </td><td> Greek plain</td></tr>
 <tr><td>greeks </td><td> Greek simplex</td></tr>
 <tr><td>italicc </td><td>  Italian complex</td></tr>
 <tr><td>italiccs </td><td> Italian complex small</td></tr>
 <tr><td>italict </td><td> Italian triplex</td></tr>
 <tr><td>romanc </td><td> Roman complex</td></tr>
 <tr><td>romancs </td><td> Roman complex small</td></tr>
 <tr><td>romand </td><td> Roman duplex</td></tr>
 <tr><td>romanp </td><td> Roman plain</td></tr>
 <tr><td>romans </td><td> Roman simplex</td></tr>
 <tr><td>romant </td><td> Roman triplex</td></tr>
 <tr><td>scriptc </td><td> Script complex</td></tr>
 <tr><td>scripts </td><td> Script simplex</td></tr>
 </table>
 *
 *  \param name
 *  \return int
 */

int REM_font(const char *name)
{
	if(!select_font(name))
	    select_font("romand");

	return 0;
}

int REM_font_freetype(const char *name)
{
	int stat;

	_send_ident(FONT_FREETYPE);
	_send_text(name);
	_get_int(&stat);

	return stat == 0;
}

int REM_charset(const char *name)
{
	int stat;

	_send_ident(CHARSET);
	_send_text(name);
	_get_int(&stat);

	return stat == 0;
}

int REM_font_freetype_release(void)
{
	int stat;

	_send_ident(FONT_FREETYPE_RELEASE);
	_get_int(&stat);

	return stat == 0;
}

int REM_panel_save(const char *name, int t, int b, int l, int r)
{
	close(creat(name, 0666));

	_send_ident(PANEL_SAVE);
	_send_text(name);
	_send_int(&t);
	_send_int(&b);
	_send_int(&l);
	_send_int(&r);
	R_stabilize();

	return 0;
}

int REM_panel_restore(const char *name)
{
	_send_ident(PANEL_RESTORE);
	_send_text(name);
	R_stabilize();

	return 0;
}

int REM_panel_delete(const char *name)
{
	_send_ident(PANEL_DELETE);
	_send_text(name);
	R_stabilize();

	unlink(name);

	return 0;
}

/*!
 * \brief initialize color arrays
 *
 * The three 256
 * member arrays, <b>red, green</b>, and <b>blue,</b> establish look-up
 * tables which translate the raw image values supplied in
 * <i>R_RGB_raster</i> to color intensity values which are then displayed on
 * the video screen. These two commands are tailor-made for imagery data coming
 * off sensors which give values in the range of 0-255.
 *
 *  \param r red
 *  \param g green
 *  \param b blue
 *  \return int
 */

int REM_set_RGB_color(unsigned char *r, unsigned char *g, unsigned char *b)
{
	_send_ident(RGB_COLORS);
	_send_char_array(256, r);
	_send_char_array(256, g);
	_send_char_array(256, b);

	return 0;
}

/*!
 * \brief draw a raster
 *
 * This is useful
 * only in fixed color mode (see <i>R_color_table_fixed</i>). Starting at
 * the current location, the <b>num</b> colors represented by the intensities
 * described in the <b>red, grn</b>, and <b>blu</b> arrays are drawn for
 * <b>nrows</b> consecutive pixel rows. The raw values in these arrays are in
 * the range of 0-255. They are used to map into the intensity maps which were
 * previously set with <i>R_set_RGB_color.</i> The <b>withzero</b> flag is
 * used to indicate whether 0 values are to be treated as a color (1) or should
 * be ignored (0). If ignored, those screen pixels in these locations are not
 * modified. This option is useful for graphic overlays.
 *
 *  \param n num
 *  \param nrows
 *  \param red
 *  \param grn
 *  \param blu
 *  \param nul withzero
 *  \return int
 */

int REM_RGB_raster(int n, int nrows,
	unsigned char *red, unsigned char *grn, unsigned char *blu,
	unsigned char *nul)
{
	int z = !!nul;
	_send_ident(RGB_RASTER);
	_send_int(&n);
	_send_int(&nrows);
	_send_char_array(n, red);
	_send_char_array(n, grn);
	_send_char_array(n, blu);
	_send_char_array(n, nul ? nul : red);
	_send_int(&z);

	return 0;
}

/*!
 * \brief Send arguments to the driver
 *
 * Sends arguments to the driver, preceded by the RASTER_CHAR opcode; 
 * the actual work is done by the driver. A raster drawing operation is
 * performed. The result is that a rectangular area of width <b>num</b> 
 * and height <b>nrows</b>, with its top-left corner at the current location,
 * is filled with <b>nrows</b> copies of the data pointed to by <b>ras</b>.
 *
 * \param num is the number of columns.
 * \param nrows is the number of rows to be drawn, all of which are identical
 *        (this is used for vertical scaling).
 * \param withzero should be true (non-zero) if zero pixels are to be drawn in
 *        color zero, false (zero) if they are to be transparent (i.e.
 *        not drawn).
 * \param ras should point to <b>num</b> bytes of data, which constitute the 
 *        pixels for a single row of a raster image.
 *
 * Example: to draw a byte-per-pixel image:
  \code
   unsigned char image[HEIGHT][WIDTH];

   for (y = 0; y < HEIGHT; y++)
   {
       R_move_abs(x_left, y_top + y);
       R_raster_char(WIDTH, 1, 1, image[y]);
   }
  \endcode
 *
 */

int REM_raster_char(int num, int nrows, int withzero, const unsigned char *ras)
{
	_send_ident(RASTER_CHAR);
	_send_int(&num);
	_send_int(&nrows);
	_send_int(&withzero);
	_send_char_array(num, ras);

	return 0;
}

int REM_raster_int(int num, int nrows, int withzero, const int *ras)
{
	_send_ident(RASTER_INT);
	_send_int(&num);
	_send_int(&nrows);
	_send_int(&withzero);
	_send_int_array(num, ras);

	return 0;
}

int REM_bitmap(int ncols, int nrows, int threshold, const unsigned char *buf)
{
	_send_ident(BITMAP);
	_send_int(&ncols);
	_send_int(&nrows);
	_send_int(&threshold);
	_send_char_array(ncols * nrows, buf);

	return 0;
}

#endif /* HAVE_SOCKET */

