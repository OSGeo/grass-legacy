#include "raster.h"
#include "graph.h"
static int first = 1 ;
static int t, b, l, r ;
static int getall(void);


/*!
 * \brief screen left edge
 *
 * Returns the pixel column number of the left edge of the screen.
 *
 *  \param void
 *  \return int
 */

int R_screen_left(void)
{
	if(first) getall() ;
	return(l) ;
}


/*!
 * \brief screen right edge
 *
 * Returns the pixel column
 * number of the right edge of the screen.
 *
 *  \param void
 *  \return int
 */

int R_screen_rite(void)
{
	if(first) getall() ;
	return(r) ;
}


/*!
 * \brief bottom of screen
 *
 * Returns the pixel row number of
 * the bottom of the screen.
 *
 *  \param void
 *  \return int
 */

int R_screen_bot(void)
{
	if(first) getall() ;
	return(b) ;
}


/*!
 * \brief top of screen
 *
 * Returns the pixel row number of
 * the top of the screen.
 *
 *  \param void
 *  \return int
 */

int R_screen_top(void)
{
	if(first) getall() ;
	return(t) ;
}

static int getall(void)
{
	_send_ident(SCREEN_TOP) ;
	_get_int(&t) ;
	_send_ident(SCREEN_BOT) ;
	_get_int(&b) ;
	_send_ident(SCREEN_LEFT) ;
	_get_int(&l) ;
	_send_ident(SCREEN_RITE) ;
	_get_int(&r) ;
	first = 0 ;

	return 0;
}
