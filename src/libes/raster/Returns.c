#include "raster.h"
#include "graph.h"

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

int R_screen_rite(void)
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

int R_screen_bot(void)
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

int R_screen_top(void)
{
	int t;
	_send_ident(SCREEN_TOP);
	_get_int(&t);
	return t;
}

