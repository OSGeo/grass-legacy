#include "raster.h"
#include "graph.h"


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

int R_get_text_box( char *sometext, int *t,int *b,int *l,int *r)
{
	_send_ident(GET_TEXT_BOX) ;
	_send_text(sometext) ;
	_get_int(t) ;
	_get_int(b) ;
	_get_int(l) ;
	_get_int(r) ;

	return 0;
}
