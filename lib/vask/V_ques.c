/***********************************************************************

NAME:		V_ques()

FUNCTION:	Allows a program to identify a prompt field, and where that
			field should be placed on the screen in the next call
			to V_call()

USAGE:		V_ques(targetptr, var_type, row, col, length) 

PARAMETERS:		union target targetptr ;
				int   var_type         ;     'i', 'l', 'f', 'd', or 's'
				int   row              ;     0 <= row <= MAX_LINE
				int   col              ;     0 <= col <= 80
				int   length           ;     0 <= length <= 80

RETURNS:	zero (0) on success and negative 1 (-1) on failure

ALGORITHM:	
		|	Check to see if request is legal and on the screen
		|		If not, return -1
		|	Make assignments
		|	return 0

CALLS:		
		none

***********************************************************************/

#include "vask.h"

/*!
 * \brief define screen question
 *
 * <i>Ctype</i> is one of int, long, float,
 * double, or char.
 * These two calls use the same syntax. V_const(~) and V_ques(~) specify that
 * the contents of memory at the address of <b>value</b> are to be displayed on
 * the screen at location <b>row, col</b> for <b>len</b> characters.
 * V_ques(~) further specifies that this screen location is a prompt field.  The
 * user will be allowed to change the field on the screen and thus change the
 * <b>value</b> itself. V_const(~) does not define a prompt field, and thus
 * the user will not be able to change these values.
 * <b>Value</b> is a pointer to an int, long, float, double, or char string.
 * <b>Type</b> specifies what type value points to: 'i' (int), 'l' (long), 'f'
 * (float), 'd' (double), or 's' (character string). <b>Row</b> is an integer
 * value of 0-22 specifying the row on the screen where the value is placed. The
 * top row on the screen is row 0. <b>Col</b> is an integer value of 0-79
 * specifying the column on the screen where the value is placed. The leftmost
 * column on the screen is column 0. <b>Len</b> specifies the number of columns
 * that the value will use.
 * Note that the size of a character array passed to V_ques(~) must be at least
 * one byte longer than the length of the prompt field to allow for NULL
 * termination. Currently, you are limited to 20 constants and 80 variables.
 * <b>Warning.</b> These routines store the address of <b>value</b> and not
 * the value itself. This implies that different variables must be used for
 * different calls. Programmers will instinctively use different variables with
 * V_ques(~), but it is a stumbling block for V_const(~). Also, the programmer
 * must initialize <b>value</b> prior to calling these
 * routines.\remarks{Technically <b>value</b> needs to be initialized before
 * the call to V_call(~) since V_const(~) and V_ques(~) only store the
 * address of <b>value.</b> V_call(~) looks up the values and places them on
 * the screen.}
 *
 *  \param value
 *  \param type
 *  \param row
 *  \param col
 *  \param len
 *  \return int
 */

int V_ques(
	void  *src   ,
	char  var_type ,
	int   row      ,
	int   col      ,
	int   length   )
{
	union target targetptr ;
	targetptr.i = src ;

	if (V__.NUM_ANSW >= MAX_ANSW ) 
	{
		V_error("Too many questions in call to V_ques") ;
		return(-1) ;
	}
	if ((row < 0) || (row >= MAX_LINE))
	{
		char msg[80];
		sprintf (msg,"Illegal row (%d) in call to V_ques", row);
		V_error(msg) ;
		return(-1) ;
	}
	if ((col < 0) || (col > 80))
	{
		char msg[80];
		sprintf (msg,"Illegal column (%d) in call to V_ques", col);
		V_error(msg) ;
		return(-1) ;
	}
	if ((length <= 0) || ((length + col) > 80))
	{
		V_error("Length out of bounds in call to V_ques") ;
		return(-1) ;
	}
	if ((var_type == 's') || (var_type == 'i') || (var_type == 'f')
	||  (var_type == 'l') || (var_type == 'd'))
	{
		V__.usr_answ[V__.NUM_ANSW].targetptr = targetptr ;
		V__.usr_answ[V__.NUM_ANSW].var_type  = var_type  ;
		V__.usr_answ[V__.NUM_ANSW].row       = row       ;
		V__.usr_answ[V__.NUM_ANSW].col       = col       ;
		V__.usr_answ[V__.NUM_ANSW].length    = length    ;
		V__.usr_answ[V__.NUM_ANSW].decimal_places    = V__.decimal_places    ;

		V__.NUM_ANSW++ ;
		return(0) ;
	}

	else
	{
		V_error("Illegal variable type in call to V_ques") ;
		return(-1) ;
	}
}
