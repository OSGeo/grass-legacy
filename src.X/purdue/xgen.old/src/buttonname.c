#include "xgen.h"

ReplaceButtonName(s,name)
	char *s;
	char *name;
{
	char *ptr;

	ptr = rindex(s,'=');
	ptr++;
	while( *name && isspace(*name)) name++;
	while( *name ) {
		if ( *name != '\n' )
		    *ptr++ = *name++;
		else {
			name++;
			*ptr++ = ' ';
	    }
	}
	*ptr = '\0';
}
