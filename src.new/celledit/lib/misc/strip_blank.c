/**********************************************************************
File     	:	strip_blanks.c
Function 	:	int StripBlanks (rawStr)
Args	 	:	    char *rawStr; -- string to strip
Author   	:	Mark Smith
Creation 	:	3 July 1985
Last Revised	:	26 December 1989 -- spanki@ced.berkeley.edu
Abstract     	:	Function to remove leading and trailing spaces 
			and tabs from a string.
Returns  	:      	The Length of the output string.

***********************************************************************/
# include <stdio.h>
# include <strings.h>
# include <ctype.h>

int StripBlanks(rawStr)
    char    *rawStr;            /*  String to be stripped  */

    {
    char    *rawPtr;
    char    *outPtr;
    char    outStr [256];

    strcpy (outStr, "\0");
    rawPtr = rawStr;
    outPtr = outStr;

    while (((isspace (*rawPtr)) || (iscntrl (*rawPtr))) && (*rawPtr != '\0'))
    rawPtr++;      

    while (*rawPtr != '\0')
    *outPtr++ = *rawPtr++;

    while (((isspace (*(outPtr-1))) || (iscntrl (*(outPtr-1)))) && 
    (outPtr >= outStr))
     outPtr--;

    *outPtr = '\0';

    strcpy (rawStr, outStr);
    return (strlen (rawStr));
    }
