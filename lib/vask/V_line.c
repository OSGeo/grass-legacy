/***********************************************************************

NAME:		V_line()

FUNCTION:	Allows calling program to set a line of text for the next 
			call to V_call() 

USAGE:		V_line(linenumber, text)

PARAMETERS:
			int linenumber ;
			char *text ;

RETURNS:	zero (0) on success and negative 1 (-1) on failure

ALGORITHM:	
		|	If the request is not legal return -1
		|	Otherwise, accept the request

CALLS:		

***********************************************************************/

#include "vask.h"

/*!
 * \brief add line of text to screen
 *
 * This
 * routine is used to place lines of text on the screen. <b>Row</b> is an
 * integer value of 0-22 specifying the row on the screen where the <b>text</b>
 * is placed. The top row on the screen is row 0.
 * <b>Warning.</b> V_line(~) does not copy the text to the screen description.
 * It only saves the text address. This implies that each call to V_line(~) must
 * use a different text buffer.
 *
 *  \param num
 *  \param text
 *  \return int
 */

int V_line(
	register int linenumber ,
	register char *text )
{
	if (linenumber >= MAX_LINE || linenumber < 0) 
	{
		V_error("Linenumber out of bounds in call to V_line") ;
		return(-1) ;
	}
	V__.page.line[linenumber] = text ;
	return 0;
}
