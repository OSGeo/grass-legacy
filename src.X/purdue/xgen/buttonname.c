/***************************************************************
 * buttonname.c
 *
 * this file contains the function that replaces "buttonname"
 * in set procedures with the buttons label.
 **************************************************************/
#include "xgen.h"

/***************************************************************
 * ReplaceButtonName - replace everything after = in s with name
 **************************************************************/
ReplaceButtonName(s,name)
	char *s;
	char *name;
{
	char *ptr;

	/* set ptr to the '=' in s */
	ptr = rindex(s,'=');
	/* increment past the '=' */
	ptr++;
	/****************************************************************
	 * while not at the end of the string increment past 
	 * leading whitespace in name .
     ****************************************************************/
	while( *name && isspace(*name)) name++;
	/****************************************************************
	 * while not at the end of the string 
     ****************************************************************/
	while( *name ) {
		/* and this isn't a newline */
		if ( *name != '\n' )
			/* replace the first element with that in name */
		    *ptr++ = *name++;
		/* this IS a newline, replace it with a space */
		else {
			name++;
			*ptr++ = ' ';
	    }
	}
	/* null terminate */
	*ptr = '\0';
}
