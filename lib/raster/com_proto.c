
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

int R_screen_left(void)
{
	return trans->screen_left();
}

/*!
 * \brief screen right edge
 *
 * Returns the coordinate of the right edge of the screen.
 *
 *  \param void
 *  \return int
 */

int R_screen_rite(void)
{
	return trans->screen_rite();
}

/*!
 * \brief bottom of screen
 *
 * Returns the coordinate of the bottom of the screen.
 *
 *  \param void
 *  \return int
 */

int R_screen_bot(void)
{
	return trans->screen_bot();
}


/*!
 * \brief top of screen
 *
 * Returns the coordinate of the top of the screen.
 *
 *  \param void
 *  \return int
 */

int R_screen_top(void)
{
	return trans->screen_top();
}

void R_get_num_colors(int *n)
{
	trans->get_num_colors(n);
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

void R_standard_color(int index)
{
	trans->standard_color(index);
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

void R_RGB_color(unsigned char red, unsigned char grn, unsigned char blu)
{
	trans->RGB_color(red, grn, blu);
}

/*!
 * \brief change the width of line
 *
 * Changes the <b>width</b> of line to be used in subsequent draw commands.
 *
 *  \param width
 *  \return int
 */

void R_line_width(int width)
{
	trans->line_width(width);
}

/*!
 * \brief erase screen
 *
 * Erases the entire screen to black.
 *
 *  \param void
 *  \return int
 */

void R_erase(void)
{
	trans->erase();
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

void R_move_abs(int x, int y)
{
	trans->move_abs(x, y);
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

void R_move_rel(int x, int y)
{
	trans->move_rel(x, y);
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

void R_cont_abs(int x, int y)
{
	trans->cont_abs(x, y);
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

void R_cont_rel(int x, int y)
{
	trans->cont_rel(x, y);
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

void R_polydots_abs(int *xarray, int *yarray, int number)
{
	trans->polydots_abs(xarray, yarray, number);
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

void R_polydots_rel(int *xarray, int  *yarray, int number)
{
	trans->polydots_rel(xarray, yarray, number);
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

void R_polyline_abs(int *xarray, int  *yarray, int number)
{
	trans->polyline_abs(xarray, yarray, number);
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

void R_polyline_rel(int *xarray, int *yarray, int number)
{
	trans->polyline_rel(xarray, yarray, number);
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

void R_polygon_abs(int *xarray, int *yarray, int number)
{
	trans->polygon_abs(xarray, yarray, number);
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

void R_polygon_rel(int *xarray, int *yarray, int number)
{
	trans->polygon_rel(xarray, yarray, number);
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

void R_box_abs(int x1, int y1, int x2, int y2)
{
	trans->box_abs(x1, y1, x2, y2);
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

void R_box_rel(int x, int y)
{
	trans->box_rel(x, y);
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

void R_text_size(int width, int height)
{
	trans->text_size(width, height);
}

void R_text_rotation(float rotation)
{
	trans->text_rotation(rotation);
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

void R_set_window(int t, int b, int l, int r)
{
	trans->set_window(t, b, l, r);
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

void R_text(const char *text)
{
	trans->text(text);
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

void R_get_text_box(const char *text, int *t, int *b, int *l, int *r)
{
	trans->get_text_box(text, t, b, l, r);
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

void R_font(const char *name)
{
	trans->font(name);
}

void R_font_freetype(const char *name)
{
	trans->font_freetype(name);
}

void R_charset(const char *name)
{
	trans->charset(name);
}

void R_panel_save(const char *name, int t, int b, int l, int r)
{
	trans->panel_save(name, t, b, l, r);
}

void R_panel_restore(const char *name)
{
	trans->panel_restore(name);
}

void R_panel_delete(const char *name)
{
	trans->panel_delete(name);
}

/*!
 * \brief draw a raster
 *
 * This is useful
 * only in fixed color mode (see <i>R_color_table_fixed</i>). Starting at
 * the current location, the <b>num</b> colors represented by the intensities
 * described in the <b>red, grn</b>, and <b>blu</b> arrays are drawn for
 * <b>nrows</b> consecutive pixel rows. The values in these arrays are in
 * the range of 0-255. The <b>withzero</b> flag is
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

void R_RGB_raster(int n, int nrows,
	unsigned char *red, unsigned char *grn, unsigned char *blu,
	unsigned char *nul)
{
	trans->RGB_raster(n, nrows, red, grn, blu, nul);
}

void R_begin_scaled_raster(int s[2][2], int d[2][2])
{
	return trans->begin_scaled_raster(s, d);
}

int R_scaled_raster(int n, int row,
	unsigned char *red, unsigned char *grn, unsigned char *blu,
	unsigned char *nul)
{
	return trans->scaled_raster(n, row, red, grn, blu, nul);
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


void R_bitmap(int ncols, int nrows, int threshold, const unsigned char *buf)
{
	trans->bitmap(ncols, nrows, threshold, buf);
}

