/*
*
*  Written by the GRASS Team in the Winter of 88.
*
*/

extern int WNO ;

Erase()
{
	int top, bot, left, rite ;

	Screen_top(&top) ;
	Screen_bot(&bot) ;
	Screen_rite(&rite) ;
	Screen_left(&left) ;

	rectf( WNO, left, top, rite, bot) ;

/***  old way
	Box_abs(left, top, rite, bot) ;
***/

}
