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
