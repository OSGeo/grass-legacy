static char                     rcsid[] = "@(#)XGRASS $Id: xc.assign.c,v 0.0.0.1 1992/05/05 14:59:06 kurt Exp kurt $";
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

AssignComDefs(Global)
XclipGlobalData * Global;
{
    XCCommandDefaults              *curComDef = &_xc_com_defs;

    while (curComDef) {
        if (curComDef->isFlag) {
            XCInterfaceData                     *cur;
	    
	    cur = _XcGetFlagByName(Global->_xc_Data.data, curComDef->name);

            if (cur->flag != NULL) {
                cur->flag->def = ISONSTR(curComDef->value);
                cur->flag->flagSet = True;
                cur->flag->answer = cur->flag->def;
            } else {
                sprintf(errorbuf, "no such flag name \"%s\"", curComDef->name);
                XCFatalError("assigning command line defaults", errorbuf);
            }
        } else {
            XCInterfaceData                     *cur;
	    
	    cur = _XcGetParmByName(Global->_xc_Data.data, curComDef->name);

            if (cur->parm != NULL) {
                if (cur->parm->parmSet) {
                    sprintf(errorbuf, "overriding default for parm \"%s\"",
                            cur->name);
                    XCWarning("assigning command line defaults", errorbuf);
                }
                if (cur->parm->selecttype == XC_SELECT_UNDETERMINED) {
                    /* KAB handle selectable types... */
                    ;
                }
                switch (cur->parm->type) {
                case XC_TYPE_CHARACTER:
                    cur->parm->def.cval = _XgStrDup(curComDef->value);
                    cur->parm->value.cval = cur->parm->def.cval;
                    break;
                case XC_TYPE_ENUMERATE:
                    cur->parm->def.cval = 
			_XgStrDup(_XcEnumStringToKey(cur->parm->modifier,
				  curComDef->value));
                    cur->parm->value.cval = cur->parm->def.cval;
                    break;
                case XC_TYPE_INTEGER:
                    cur->parm->def.ival = atoi(curComDef->value);
                    cur->parm->value.ival = cur->parm->def.ival;
                    break;
                case XC_TYPE_FLOAT:
#ifndef mips
                    cur->parm->def.dval =
                        strtod(curComDef->value, (char **) NULL);
#else
                    cur->parm->def.dval =
                        atof(curComDef->value);
#endif
                    cur->parm->value.dval = cur->parm->def.dval;
                    break;
                case XC_TYPE_DOUBLE:
#ifndef mips
                    cur->parm->def.dval =
                        strtod(curComDef->value, (char **) NULL);
#else
                    cur->parm->def.dval =
                        atof(curComDef->value);
#endif
                    cur->parm->value.dval = cur->parm->def.dval;
                    break;
                case XC_TYPE_LOGICAL:
                    cur->parm->def.bval = (ISONSTR(curComDef->value) ||
					ISTRUESTR(curComDef->value));
                    cur->parm->value.bval = cur->parm->def.bval;
                    break;
                case XC_TYPE_FILENAME:
                    cur->parm->def.cval = _XgStrDup(curComDef->value);
                    cur->parm->value.cval = _XgStrDup(curComDef->value);
                    if (cur->parm->isInput &&
                            access(cur->parm->def.cval, 0) != 0) {
                        sprintf(errorbuf, "invalid path name",
                                curComDef->value);
                        XCFatalError("assigning command line defaults",
                                   errorbuf);
                    }
                    break;
                case XC_TYPE_DB_ELEMENT:
                    cur->parm->def.dbval.desc = _XgStrDup(curComDef->value);
                    cur->parm->def.dbval.type = cur->parm->value.dbval.type;
                    cur->parm->def.dbval.status = cur->parm->value.dbval.status;
                    cur->parm->value.dbval.desc = cur->parm->def.dbval.desc;
                    break;
                }
                cur->parm->parmSet = True;
                if (cur->parm->isInput &&
                        cur->parm->type == XC_TYPE_DB_ELEMENT)
                    VerifyDBElement(cur->parm);
            } else {
		sprintf(errorbuf, "no such parameter name \"%s\"", 
		    curComDef->name);
		XCFatalError("assigning command line defaults", errorbuf);
	    }
        }
        curComDef = curComDef->next;
    }
}
