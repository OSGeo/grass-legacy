/**********************************************************************
   check.c      - check value type for consistency
 *********************************************************************/
/*******************************************************************************
Xgen was developed by Kurt Buehler, while at the Center for Advanced Decision
Support for Water and Environmental Systems (CADSWES), University of Colorado
at Boulder and at the Indiana Water Resources Research Center (IWRRC),
Purdue University for the U.S. Army Construction Engineering Research
Laboratory in support of the Geographical Resources Analysis Support
System (GRASS) software. The example scripts were developed by Ms. Christine
Poulsen of USA-CERL, much thanks goes to her for her work.

Permission to use, copy, modify and distribute without charge this software,
documentation, etc. is granted, provided that this comment is retained,
and that the names of Kurt Buehler, Christine Poulsen, CADSWES, IWRRC,
the University of Colorado at Boulder, Purdue University, or USA-CERL are not
used in advertising or publicity pertaining to distribution of the software
without specific, written prior permission.

The author disclaims all warranties with regard to this software, including
all implied warranties of merchantability and fitness, in no event shall
the author be liable for any special, indirect or consequential damages or
any damages whatsoever resulting from loss of use, data or profits,
whether in an action of contract, negligence or other tortious action,
arising out of or in connection with the use or performance of this
software.
*******************************************************************************/
#include "xgen.h"

Boolean
CheckType(value, expected)
    char                           *value;
    DataType                        expected;
{
    switch (expected) {
    case OnOff:
        if (!(!strcmp(value, "On") ||
              !strcmp(value, "on") ||
              !strcmp(value, "True") ||
              !strcmp(value, "true") ||
              !strcmp(value, "Off") ||
              !strcmp(value, "off") ||
              !strcmp(value, "False") ||
              !strcmp(value, "false"))) {
            sprintf(errorbuf, "[%s] not on/off or true/false", value);
            return False;
        }
        break;
    case Real:
        {
#ifdef SVR4
	char *garb;
	double val;

        val = strtod(value, &garb);
        if (*garb) {
	    sprintf(errorbuf,
		 "[%s] extra characters in real number", garb);
	    return False;
        }
#else
#ifdef BSD
        char *sPtr = value;
        while ( *sPtr ) {
            if ( !isdigit(*sPtr)) {
		sprintf(errorbuf,
		     "[%s] extra characters in real number", value);
                return False;
            }
            sPtr++;
        }
#else
	char *garb;
	double val;

        val = strtod(value, &garb);
        if (*garb) {
	    sprintf(errorbuf,
		 "[%s] extra characters in real number", garb);
	    return False;
        }
#endif
#endif
        }
        break;
    case Int:
        {
            char                           *tmp = value;
	    Boolean                         first = True;

            while (*tmp) {
		if ( first ) {
		    if ( !isdigit(*tmp) && *tmp++ != '-' ) {
			sprintf(errorbuf,
				"[%s] not an integer number", value);
			return False;
		    }
		    first = False;
		} else 
                if (!isdigit(*tmp++)) {
                    sprintf(errorbuf,
                            "[%s] not an integer number", value);
                    return False;
                }
            }
        }
        break;
    }
    return True;
}
