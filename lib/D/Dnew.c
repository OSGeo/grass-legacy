#include <grass/raster.h>
#include <grass/display.h>


/*!
 * \brief
 *
 * Create a new display window <b>name</b>
 *
 *  \param name
 *  \param bot
 *  \param top
 *  \param left
 *  \param right
 *  \return int
 */

int Dnew(char *name, float bot,float top,float left,float right)
{
	int scr_top, scr_bot, scr_left, scr_rite ;
	int iTOP, iBOTTOM, iLEFT, iRIGHT ;
	int stat ;

	scr_top  = R_screen_top ();
	scr_bot  = R_screen_bot ();
	scr_left = R_screen_left ();
	scr_rite = R_screen_rite ();

	iTOP    = scr_top + (scr_bot - scr_top) * (100. - top) / 100.0 ;
	iBOTTOM = scr_top + (scr_bot - scr_top) * (100. - bot) / 100.0 ;
	iLEFT   = scr_left + (scr_rite - scr_left) * left / 100.0 ;
	iRIGHT   = scr_left + (scr_rite - scr_left) * right / 100.0 ;

	if (iTOP    < R_screen_top()) iTOP    = R_screen_top() ;
	if (iBOTTOM > R_screen_bot()) iBOTTOM = R_screen_bot() ;
	if (iLEFT   < R_screen_left()) iLEFT   = R_screen_left() ;
	if (iRIGHT  > R_screen_rite()) iRIGHT  = R_screen_rite() ;

	stat = D_new_window(name, iTOP, iBOTTOM, iLEFT, iRIGHT) ;

	return(stat) ;
}

