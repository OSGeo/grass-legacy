/***********************************************************************

NAME:		V_clear()

FUNCTION:	Effectively zero out the prompt and answer arrays.

USAGE:		V_clear()

PARAMETERS:

RETURNS:	nothing

ALGORITHM:	
		|	set lengths of all answer locations to zero (0)
		|	set lengths of all constant locations to zero (0)
		|	set all text strings to NULL (\0)
		|	set number of constants to 0
		|	set number of answers to 0
		|	set number of text lines to 0
		|	set number of decimal places to 2
		|	resets interrupt msg 

CALLS:		

***********************************************************************/
#include "vask.h"

#define DECIMAL_PLACES -1


/*!
 * \brief initialize screen description
 *
 * This routine initializes
 * the screen description information, and must be called before each new screen
 * layout description.
 *
 *  \return int
 */

int V_clear()
{
	static char *text = "" ;
	int at_answer ;

	for (at_answer=0; at_answer < MAX_ANSW; at_answer++)  
		V__.usr_answ[at_answer].length = 0 ;

	for (at_answer=0; at_answer < MAX_CONST; at_answer++)  
		V__.constant[at_answer].length = 0 ;

	for (at_answer=0; at_answer < MAX_LINE; at_answer++) 
		V__.page.line[at_answer] = text ;
	
	V__.NUM_CONST = 0 ;
	V__.NUM_ANSW  = 0 ;
	V__.NUM_LINE  = 0 ;
	V_float_accuracy(DECIMAL_PLACES);
	sprintf (V__.interrupt_msg, "CANCEL");

/* please leave this code commented out 
	fflush(stdout);
	system("clear");
*/

	return 0;
}
