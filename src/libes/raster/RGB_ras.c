#include "raster.h"
#include "graph.h"


/*!
 * \brief initialize graphics
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

int R_set_RGB_color(unsigned char *r,unsigned char *g,unsigned char *b)
{
	_send_ident(RGB_COLORS) ;
	_send_char_array(256,r) ;
	_send_char_array(256,g) ;
	_send_char_array(256,b) ;

	return 0;
}


/*!
 * \brief draw a raster
 *
 * This is useful
 * only in fixed color mode (see <i>R_color_table_fixed</i>). Starting at
 * the current position, the <b>num</b> colors represented by the intensities
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

int R_RGB_raster(int n, int nrows,
	unsigned char *red, unsigned char *grn, unsigned char *blu,
	unsigned char *nul)
{
	int z ;
	_send_ident(RGB_RASTER) ;
	z = n ;
	_send_int(&z) ;
	z = nrows ;
	_send_int(&z) ;
	_send_char_array(n,red) ;
	_send_char_array(n,grn) ;
	_send_char_array(n,blu) ;
	_send_char_array(n,nul ? nul : red) ;
	z = (int) nul ;
	_send_int(&z) ;

	return 0;
}
