static char rcsid[] = "@(#)XGRASS $Id: xc.name.c,v 0.0.0.1 1992/05/05 14:59:04 kurt Exp kurt $";
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

/* I guess a hash table is really in order, but we don't expect to have
   *that* many flags and parameters, so the search will be short*/


XCInterfaceData *
_XcGetParmByParmData(cur,p)
XCInterfaceData *cur;
XCParmData *p;
{
    XCInterfaceData *ret;

    while ( cur ) {
        if ( cur->type == XC_PARM && p == cur->parm ) return cur;
        if ( cur->type == XC_DIALOG ) {
            if ( (ret = _XcGetParmByParmData(cur->right,p)) != NULL )
                return ret;
        }
        cur = cur->left;
    }
    return NULL;
}

XCInterfaceData *
_XcGetFlagByFlagData(cur,f)
XCInterfaceData *cur;
XCFlagData *f;
{
    XCInterfaceData *ret;

    while ( cur ) {
        if ( cur->type == XC_FLAG && f == cur->flag ) return cur;
        if ( cur->type == XC_DIALOG ) {
            if ( (ret = _XcGetFlagByFlagData(cur->right,f)) != NULL )
                return ret;
        }
        cur = cur->left;
    }
    return NULL;
}

XCInterfaceData *
_XcGetParmByName(cur,s)
XCInterfaceData *cur;
char *s;
{
    XCInterfaceData *ret;

    while ( cur ) {
        if ( cur->type == XC_PARM && !strcmp(s,cur->name) ) return cur;
        if ( cur->type == XC_DIALOG ) {
            if ( (ret = _XcGetParmByName(cur->right,s)) != NULL )
                return ret;
        }
        cur = cur->left;
    }
    return NULL;
}

XCInterfaceData *
_XcGetFlagByName(cur,s)
XCInterfaceData *cur;
char *s;
{
    XCInterfaceData *ret;

    while ( cur ) {
	if ( cur->type == XC_FLAG && !strcmp(s,cur->name) ) return cur;
	if ( cur->type == XC_DIALOG ) {
	    if ( (ret = _XcGetFlagByName(cur->right,s)) != NULL )
		return ret;
	}
	cur = cur->left;
    }
    return NULL;
}

Boolean
_XcUniqueName(cur,s)
XCInterfaceData *cur;
char *s;
{
    while ( cur ) {
	if ( cur->type == XC_DIALOG ) {
	    if ( !_XcUniqueName(cur->right,s) ) return False;
	} else if ( cur->name != NULL ) 
	    if ( !strcmp(s,cur->name ) ) return False;

	cur = cur->left;
    }
    return True;
}

/* does the opposite of _XcUniqueName */

Boolean
_XcIsName(cur, s, type)
XCInterfaceData *cur;
char *s;
int type;
{
    while ( cur ) {
	if ( cur->type == XC_DIALOG ) {
	    if ( _XcIsName(cur->right,s,type) ) return True;
	} else if ( cur->name != NULL ) 
	    if ( !strcmp(s,cur->name ) && cur->type == type ) return True;

	cur = cur->left;
    }
    return False;
}

/*
 * make the code that follows easier to read...
 */

#define ERR1 \
"invalid or non-existant flag name \"%s\"\n\tspecified in requirement for flag\"%s\""

#define ERR2 \
"invalid or non-existant parameter name \"%s\"\n\tspecified in requirement for flag \"%s\""

#define ERR3 \
"invalid or non-existant flag name \"%s\"\n\tspecified in dependency for flag \"%s\""

#define ERR4 \
"invalid or non-existant parameter name \"%s\"\n\tspecified in dependency for flag \"%s\""

#define ERR5 \
"invalid or non-existant flag name \"%s\"\n\tspecified in requirement for parameter \"%s\""

#define ERR6 \
"invalid or non-existant parameter name \"%s\"\n\tspecified in requirement for parameter \"%s\""

#define ERR7 \
"invalid or non-existant flag name \"%s\"\n\tspecified in dependency for parameter \"%s\""

#define ERR8 \
"invalid or non-existant parameter name \"%s\"\n\tspecified in dependency for parameter \"%s\""

XCRequireData *
_XcReturnAppropriateRequireData(cur)
XCInterfaceData *cur;
{
    if ( cur->type == XC_FLAG )
	return(&cur->flag->requires);
    return(&cur->parm->requires);
}

XCRequireData *
_XcReturnAppropriatePrecludeData(cur)
XCInterfaceData *cur;
{
    if ( cur->type == XC_FLAG )
	return((cur->flag->preclude.name)?&cur->flag->preclude:NULL);
    return((cur->parm->preclude.name)?&cur->parm->preclude:NULL);
}

_XcValidDependencies(cur,Global)
XCInterfaceData *cur;
XclipGlobalData *Global;
{
    XCRequireData *rPtr;
    Boolean foundError = False;

    while ( cur ) {
	if ( cur->type == XC_DIALOG ) _XcValidDependencies(cur->right,Global);
	if ( (cur->type == XC_FLAG && cur->flag->requirements) ||
	      (cur->type == XC_PARM && cur->parm->requirements)) {
	    rPtr = _XcReturnAppropriateRequireData(cur);

	    while ( rPtr ) {
		if ( rPtr->isFlag && 
		     (!_XcIsName(Global->_xc_Data.data, rPtr->name, XC_FLAG) &&
		      !_XcIsName(Global->_xc_Data.data, rPtr->name, XC_PARM))) {
		    if ( cur->type == XC_FLAG )
			sprintf(errorbuf, ERR1, rPtr->name, cur->name);
		    else
		        sprintf(errorbuf, ERR5, rPtr->name, cur->name);
		    XCError("validating dependencies", errorbuf, Global);
		    foundError = True;
		} else if ( !rPtr->isFlag && 
			    (!_XcIsName(Global->_xc_Data.data, rPtr->name, XC_PARM) &&
			     !_XcIsName(Global->_xc_Data.data, rPtr->name, XC_FLAG))) {
		    if ( cur->type == XC_FLAG )
			sprintf(errorbuf, ERR2, rPtr->name, cur->name);
		    else
			sprintf(errorbuf, ERR6, rPtr->name, cur->name);
		    XCError("validating dependencies", errorbuf, Global);
		    foundError = True;
		}
		rPtr = rPtr->next;
	    }
	} else if ( (cur->type == XC_FLAG && cur->flag->precludes) ||
	      (cur->type == XC_PARM && cur->parm->precludes)) {
	    rPtr = _XcReturnAppropriatePrecludeData(cur);

	    while ( rPtr ) {
		if ( rPtr->isFlag && 
		      (!_XcIsName(Global->_xc_Data.data, rPtr->name, XC_FLAG) &&
		       !_XcIsName(Global->_xc_Data.data, rPtr->name, XC_PARM))) {
		    if ( cur->type == XC_FLAG )
			sprintf(errorbuf, ERR3, rPtr->name, cur->name);
		    else
			sprintf(errorbuf, ERR4, rPtr->name, cur->name);
		    XCError("validating dependencies", errorbuf, Global);
		    foundError = True;
		} else if ( !rPtr->isFlag && 
			    (!_XcIsName(Global->_xc_Data.data, rPtr->name, XC_PARM) &&
			     !_XcIsName(Global->_xc_Data.data, rPtr->name, XC_FLAG))) {
		    if ( cur->type == XC_FLAG )
			sprintf(errorbuf, ERR7, rPtr->name, cur->name);
		    else
			sprintf(errorbuf, ERR8, rPtr->name, cur->name);
		    XCError("validating dependencies", errorbuf, Global);
		    foundError = True;
		}
		rPtr = rPtr->next;
	    }
	}
	cur = cur->left;
    }
    if ( foundError && Global->standAlone) exit(1);
}


char *
_XcGetStringValueByName(cur, s)
XCInterfaceData *cur;
char *s;
{
    char *ret;

    while ( cur ) {
	if ( cur->type == XC_DIALOG ) {
	    if ( (ret = _XcGetStringValueByName(cur->right,s)) != NULL )
		return ret;
	}
        if ( cur->name != NULL ) {
            if ( !strcmp(s,cur->name ) ) {
                if ( cur->type == XC_FLAG && cur->flag->flagSet && 
		     cur->flag->answer ) {
                    return cur->flag->key;
                } else if ( cur->type == XC_PARM && cur->parm->parmSet ) {
                    static char retBuf[256];

                    switch(cur->parm->type) {
                    case  XC_TYPE_CHARACTER:
                        sprintf(retBuf,"%s",cur->parm->value.cval);
                        return retBuf;
                        break;
                    case  XC_TYPE_ENUMERATE:
                        sprintf(retBuf,"%s",cur->parm->value.cval);
                        return retBuf;
                        break;
                    case  XC_TYPE_INTEGER:
			if ( cur->parm->hasModifier ) {
			    if ( cur->parm->togSet ) 
				sprintf(retBuf,"%d",cur->parm->value.ival);
			    else
				return NULL;
			} else {
			    sprintf(retBuf,"%d",cur->parm->value.ival);
			}
                        return retBuf;
                        break;
                    case  XC_TYPE_FLOAT:
			if ( cur->parm->hasModifier ) {
			    if ( cur->parm->togSet ) 
				sprintf(retBuf,"%f",cur->parm->value.dval);
			    else
				return NULL;
			} else {
			    sprintf(retBuf,"%f",cur->parm->value.dval);
			}
                        return retBuf;
                        break;
                    case  XC_TYPE_DOUBLE:
			if ( cur->parm->hasModifier ) {
			    Boolean _isset;

			    XtVaGetValues(cur->parm->scale_toggle,
				XmNset, &_isset, NULL);
			    if ( _isset ) 
				sprintf(retBuf,"%lf",cur->parm->value.dval);
			    else
				return NULL;
			} else {
			    sprintf(retBuf,"%lf",cur->parm->value.dval);
			}
                        return retBuf;
                        break;
                    case  XC_TYPE_LOGICAL:
                        if ( cur->parm->hasModifier )  {
                            if ( cur->parm->value.bval )
                                sprintf(retBuf,"%s", 
				    cur->parm->modifier.onString);
                            else
                                sprintf(retBuf,"%s", 
				    cur->parm->modifier.offString);
                        } else {
                            if ( cur->parm->value.bval )
                                sprintf(retBuf,"on");
                            else
                                sprintf(retBuf,"off");
                        }
                        return retBuf;
                        break;
                    case  XC_TYPE_FILENAME:
                        sprintf(retBuf,"%s",cur->parm->value.dbval.filename);
                        return retBuf;
                        break;
                    case  XC_TYPE_DB_ELEMENT:
                        sprintf(retBuf,"%s",cur->parm->value.dbval.desc);
                        return retBuf;
                        break;
                    }
		}
            }
	}
        cur = cur->left;
    }
    return NULL;
}

