
#include <stdio.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

#include <grass/gis.h>
#include <grass/raster.h>
#include <grass/graphics.h>

#include <grass/freetypecap.h>

#include "driver.h"
#include "transport.h"

/*!
 * \brief screen left edge
 *
 * Returns the coordinate of the left edge of the screen.
 *
 *  \param void
 *  \return int
 */

int LOC_screen_left(void)
{
	int l;

	COM_Screen_left(&l);

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

int LOC_screen_rite(void)
{
	int r;

	COM_Screen_rite(&r);

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

int LOC_screen_bot(void)
{
	int b;

	COM_Screen_bot(&b);

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

int LOC_screen_top(void)
{
	int t;

	COM_Screen_top(&t);

	return t;
}

int LOC_get_num_colors(int *n)
{
	COM_Number_of_colors(n);

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

int LOC_color_table_float(void)
{
	return COM_Color_table_float();
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

int LOC_color_table_fixed(void)
{
	return COM_Color_table_fixed();
}

int LOC_color_offset(int n)
{
	COM_Color_offset(n);

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

int LOC_color(int index)
{
	COM_Color(index);

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

int LOC_standard_color(int index)
{
	COM_Standard_color(index);

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

int LOC_RGB_color(unsigned char red, unsigned char grn, unsigned char blu)
{
	COM_Color_RGB(red, grn, blu);

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

int LOC_reset_color(unsigned char red, unsigned char grn, unsigned char blu,
		  int index)
{
	if (index < 0)
		index = 256 - index;

	COM_Reset_color(red, grn, blu, index);

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

int LOC_reset_colors(int min, int max,
		   unsigned char *red, unsigned char *grn, unsigned char *blu)
{
	COM_Reset_colors(min, max, red, grn, blu);

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

int LOC_line_width(int width)
{
	COM_Line_width(width);

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

int LOC_erase(void)
{
	COM_Erase();

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

int LOC_move_abs(int x, int y)
{
	COM_Move_abs(x, y);

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

int LOC_move_rel(int x, int y)
{
	COM_Move_rel(x, y);

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

int LOC_cont_abs(int x, int y)
{
	COM_Cont_abs(x, y);

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

int LOC_cont_rel(int x, int y)
{
	COM_Cont_rel(x, y);

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

int LOC_polydots_abs(int *xarray, int *yarray, int number)
{
	COM_Polydots_abs(xarray, yarray, number);

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

int LOC_polydots_rel(int *xarray, int  *yarray, int number)
{
	COM_Polydots_rel(xarray, yarray, number);

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

int LOC_polyline_abs(int *xarray, int  *yarray, int number)
{
	COM_Polyline_abs(xarray, yarray, number);

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

int LOC_polyline_rel(int *xarray, int *yarray, int number)
{
	COM_Polyline_rel(xarray, yarray, number);

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

int LOC_polygon_abs(int *xarray, int *yarray, int number)
{
	COM_Polygon_abs(xarray, yarray, number);

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

int LOC_polygon_rel(int *xarray, int *yarray, int number)
{
	COM_Polygon_rel(xarray, yarray, number);

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

int LOC_box_abs(int x1, int y1, int x2, int y2)
{
	COM_Box_abs(x1, y1, x2, y2);

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

int LOC_box_rel(int x, int y)
{
	COM_Box_rel(x, y);

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

int LOC_text_size(int width, int height)
{
	COM_Text_size(width, height);

	return 0;
}

int LOC_text_rotation(float rotation)
{
	COM_Text_rotation(rotation);

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

int LOC_set_window(int t, int b, int l, int r)
{
	COM_Set_window(t, b, l, r);

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

int LOC_text(const char *text)
{
	COM_Text(text);

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

int LOC_get_text_box(const char *text, int *t, int *b, int *l, int *r)
{
	COM_Get_text_box(text, t, b, l, r);

	return 0;
}

static int select_font(const char *name)
{
	char filename[4096];
	int stat;
	int i;
	extern struct FT_CAP *ftcap;
	FILE *fp;

	/* check if freetype font is available in freetypecap */
	for(i=0; ftcap[i].name && strcmp(name, ftcap[i].name) != 0; i++);

	/* use freetype font */
	if(ftcap[i].name)
		return LOC_font_freetype(ftcap[i].path);

	/* freetype font path:charset */
	if(name[0] == '/' && (fp = fopen(name, "r")))
	{
		fclose(fp);
		return LOC_font_freetype(name);
	}

	/* stroke fonts */
	sprintf(filename, "%s/fonts/%s", G_gisbase(), name);
	stat = COM_Font_get(filename);

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

int LOC_font(const char *name)
{
	if (!select_font(name))
		select_font("romand");

	return 0;
}

int LOC_font_freetype(const char *name)
{
	return COM_Font_freetype_get(name) == 0;
}

int LOC_charset(const char *name)
{
	return COM_Font_init_charset(name) == 0;
}

int LOC_font_freetype_release(void)
{
	return COM_Font_freetype_release() == 0;
}

int LOC_panel_save(const char *name, int t, int b, int l, int r)
{
	close(creat(name, 0666));

	COM_Panel_save(name, t, b, l, r);
	R_stabilize();

	return 0;
}

int LOC_panel_restore(const char *name)
{
	COM_Panel_restore(name);
	R_stabilize();

	return 0;
}

int LOC_panel_delete(const char *name)
{
	COM_Panel_delete(name);
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

int LOC_set_RGB_color(unsigned char *r, unsigned char *g, unsigned char *b)
{
	COM_RGB_set_colors(r, g, b);

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

int LOC_RGB_raster(int n, int nrows,
	unsigned char *red, unsigned char *grn, unsigned char *blu,
	unsigned char *nul)
{
	COM_RGB_raster(n, nrows, red, grn, blu, nul);

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

int LOC_raster_char(int num, int nrows, int withzero, const unsigned char *ras)
{
	COM_Raster_char(num, nrows, ras, withzero, 1);

	return 0;
}

int LOC_raster_int(int num, int nrows, int withzero, const int *ras)
{
	COM_Raster_int(num, nrows, ras, withzero, 1);

	return 0;
}

int LOC_bitmap(int ncols, int nrows, int threshold, const unsigned char *buf)
{
	COM_Bitmap(ncols, nrows, threshold, buf);

	return 0;
}

