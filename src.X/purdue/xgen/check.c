#include "xgen.h"

Boolean
CheckType(value,expected)
    char *value;
    DataType expected;
{
    switch(expected) {
        case OnOff:
            if ( !(!strcmp(value,"On") ||
                   !strcmp(value,"on") ||
                   !strcmp(value,"True") ||
                   !strcmp(value,"true") ||
                   !strcmp(value,"Off") ||
                   !strcmp(value,"off") ||
                   !strcmp(value,"False") ||
                   !strcmp(value,"false")) ) {
				sprintf(errorbuf,"[%s] not on/off or true/false",value);
                return False;
            }
            break;
        case Real:
            {
                char *place;
                double val = strtod(value,&place);
                char *saveplace = place;

                if ( val == 0.0 && !strcmp(value, place) ) {
                    sprintf(errorbuf,"[%s] not a real number",value);
                    return False;
                }
                while ( *place++ ) {
                    if ( ! isspace(*place) ) {
                        sprintf(errorbuf,
                          "[%s] extra characters in real number",saveplace);
                        return False;
                    }
                }
            }
            break;
        case Integer:
			{
                char *tmp = value;

                while( *tmp ) {
                    if ( ! isdigit(*tmp++)) {
                        sprintf(errorbuf,
					      "[%s] not an integer number",value);
                        return False;
                    }
                }
            }
            break;
    }
	return True;
}
