/**********************************************************************

Function:       StrToUpper ()
Author:         Mark Smith
Last Revised:   18 May 1988

Description:    Converts a string to upper case.

Returns:        None.

**********************************************************************/
# include	<ctype.h>


StrToUpper (raw_str)
    char     *raw_str;
    {
    char     *raw_ptr;

    raw_ptr = raw_str - 1;

    while (*(++raw_ptr) != '\0')
	if (islower(*raw_ptr))
	    *raw_ptr = toupper (*raw_ptr);
    return;
    }


