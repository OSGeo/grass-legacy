#include "raster.h"
#include "graph.h"


/*!
 * \brief fill a box
 *
 * A box is drawn in the current color using the coordinates <b>x1,y1</b> and
 * <b>x2,y2</b> as opposite corners of the box. The current location is updated
 * to <b>x2,y2.</b>
 *
 *  \param x1
 *  \param y1
 *  \param x2
 *  \param y2
 *  \return int
 */

int R_box_abs(int x1,int y1,int x2,int y2 )
{
	int z ;
	_send_ident(BOX_ABS) ;
	z = x1 ;
	_send_int(&z) ;
	z = y1 ;
	_send_int(&z) ;
	z = x2;
	_send_int(&z) ;
	z = y2;
	_send_int(&z) ;

	return 0;
}


/*!
 * \brief fill a box
 *
 * A box is drawn in the current color using the current location as one corner 
 * and the current location plus <b>x</b> and <b>y</b> as the opposite corner 
 * of the box. The current location is updated:
  \code
    Newx = Oldx + x;
    Newy = Oldy + y;
  \endcode
 *
 *  \param x
 *  \param y
 *  \return int
 */

int R_box_rel(int x,int y )
{
	int z ;
	_send_ident(BOX_REL) ;
	z = x ;
	_send_int(&z) ;
	z = y ;
	_send_int(&z) ;

	return 0;
}
