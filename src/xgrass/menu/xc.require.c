static char rcsid[] = "@(#)XGRASS $Id: xc.require.c,v 0.0.0.1 1992/05/05 14:59:10 kurt Exp kurt $";
/*
 * File:
 *
 * Desc:
 *
 * Auth:
 *
 * Date:
 *
 * Modification History:
 *
 *
 */
#include "xc.xclip.h"

#define ERR1 \
"Flag not set:\n\"%s\"\n\n<specified in requirement for flag \"%s\">"
#define ERR2 \
"Flag not set:\"%s\"\n\n<specified in requirement for parameter \"%s\">"
#define ERR3 \
"Parameter not set:\"%s\"\n\n<specified in requirement for flag \"%s\">"
#define ERR4 \
"Parameter not set:\"%s\"\n\n<specified in requirement for parameter \"%s\">"
#define ERR5 \
"Required parameter \"%s\" should be set before execution.\n"

Boolean
_XcCheckRequirements(cur,Global)
XCInterfaceData *cur;
XclipGlobalData *Global;
{
    XCRequireData *rPtr;
    XCInterfaceData *iPtr;
    Boolean foundError = False;

    while ( cur ) {
	if ( cur->type == XC_DIALOG ) {
	    if ( !_XcCheckRequirements(cur->right,Global) ) foundError = True;
	}
	if ( (cur->type == XC_FLAG && cur->flag->requirements) ||
	      (cur->type == XC_PARM && cur->parm->requirements)) {
	    rPtr = _XcReturnAppropriateRequireData(cur);

	    while ( rPtr ) {
		if ( rPtr->isFlag && 
		     (_XcGetStringValueByName(Global->_xc_Data.data,
			 cur->name) != NULL) &&
		     (_XcGetStringValueByName(Global->_xc_Data.data, 
			 rPtr->name) == NULL) ) {
		    if ( cur->type == XC_FLAG ) {
			iPtr = _XcGetFlagByName(Global->_xc_Data.data,rPtr->name);
			sprintf(errorbuf, ERR1, iPtr->flag->desc, 
			    cur->flag->desc);
		    } else {
			iPtr = _XcGetFlagByName(Global->_xc_Data.data,rPtr->name);
		        sprintf(errorbuf, ERR2, iPtr->flag->desc, 
			    cur->parm->desc);
		    }
		    XgError(Global->interactor, errorbuf);
		    foundError = True;
		} else if ( !rPtr->isFlag && 
		     (_XcGetStringValueByName(Global->_xc_Data.data,
			 cur->name) != NULL) &&
		     (_XcGetStringValueByName(Global->_xc_Data.data, 
			 rPtr->name) == NULL) ) {
		    if ( cur->type == XC_FLAG ) {
			iPtr = _XcGetParmByName(Global->_xc_Data.data,rPtr->name);
			sprintf(errorbuf, ERR3, iPtr->parm->desc, 
			    cur->flag->desc);
		    } else {
			iPtr = _XcGetParmByName(Global->_xc_Data.data,rPtr->name);
			sprintf(errorbuf, ERR4, iPtr->parm->desc, 
			    cur->parm->desc);
		    }
		    XgError(Global->interactor, errorbuf);
		    foundError = True;
		}
		rPtr = rPtr->next;
	    }
	}
	cur = cur->left;
    }
    if ( foundError ) return False;
    return True;
}

Boolean 
_XcCheckOptional(cur,Global)
XCInterfaceData *cur;
XclipGlobalData *Global;
{
    XCInterfaceData *iPtr;
    Boolean foundError = False;

    while ( cur ) {
        if ( cur->type == XC_DIALOG ) {
            if ( !_XcCheckOptional(cur->right,Global) ) foundError = True;
        } else {
            if ( cur->type == XC_PARM && !cur->parm->optional ) {
		if ( _XcGetStringValueByName(Global->_xc_Data.data,cur->name) == NULL) {
                    sprintf(errorbuf,ERR5, cur->parm->desc);
		    XgError(Global->interactor, errorbuf);
                    foundError = True;
                }
            }
        }
        cur = cur->left;       
    }
    if ( foundError ) return False;
    return True;
}



