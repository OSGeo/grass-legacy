static char                     rcsid[] = "@(#)XGRASS $Id: xc.comstr.c,v 0.0.0.1 1992/05/05 14:59:08 kurt Exp kurt $";
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

#define EMSG1 "illegal '[' in flag or parameter name \n\tcontext \"%10s\""
#define EMSG2 "illegal ']' in flag or parameter name \n\tcontext \"%10s\""
#define EMSG3 "can't nest '(' notation\n\tcontext \"%10s\""
#define EMSG4 "missing ')' in flag or parameter name \n\tcontext \"%10s\""
#define EMSG5 "missing ')'\n\tcontext \"%10s\""
#define EMSG6 "missing ']'\n\tcontext \"%10s\""

char                           *
DoParenText(arg,Global)
    char                           **arg;
    XclipGlobalData *Global;
{
    char                           *ptr = *arg;
    char                           *sptr;
   
    ptr++;
    sptr = ptr;

    while (*ptr != '\0') {
        /* oops... */
        if (*ptr == '[') {
            sprintf(errorbuf, EMSG1, ptr);
            XCError("parsing command string", errorbuf, Global);
            /* oops... */
        } else if (*ptr == ']') {
            sprintf(errorbuf, EMSG2, ptr);
            XCError("parsing command string", errorbuf, Global);
            /* oops... */
        } else if (*ptr == '(') {
            sprintf(errorbuf, EMSG3, ptr);
            XCError("parsing command string", errorbuf, Global);
            /* hey...could be good... */
        } else if (*ptr == ')') {
            char                           *tmp;
            *ptr = '\0';
            tmp = _XcGetStringValueByName(Global->_xc_Data.data, sptr);
            *ptr = ')';
            ptr++;
            *arg = ptr;
            return _XgStrDup(tmp);;
        }
        ptr++;
    }
    /* super oops... */
    sprintf(errorbuf, EMSG4, ptr);
    XCError("parsing command string", errorbuf, Global);
}

char                           *
DoBracketText(arg,Global)
    char                          **arg;
    XclipGlobalData *Global;
{
    char                           *ptr = *arg;
    char                            insert[512];
    Boolean                         hasValue = False;

    ptr++;
    insert[0] = '\0';

    while (*ptr != '\0') {
        /* eat  backquoted stuff we're looking for... */
        if (*ptr == '\\' &&
            (*(ptr + 1) == '[' || *(ptr + 1) == '(' || *(ptr + 1) == ']')) {
            _XcCharCat(insert, *ptr);
            ptr++;
            /* a nested optional expression */
        } else if (*ptr == '[') {
            char                           *tmp = DoBracketText(&ptr,Global);
            if (tmp != NULL) {
                hasValue = True;
                strcat(insert, tmp);
                _XgFree(tmp);
            }
            /* we're done with this one */
        } else if (*ptr == ']') {
            ptr++;
            *arg = ptr;
            if (hasValue)
                return _XgStrDup(insert);
            else
                return NULL;
            /* process flag or parm stuff */
        } else if (*ptr == '(') {
            char                           *tmp = DoParenText(&ptr,Global);
            if (tmp != NULL) {
                hasValue = True;
                strcat(insert, tmp);
                _XgFree(tmp);
            }
            /* stow the rest */
        } else {
            _XcCharCat(insert, *ptr);
            ptr++;
        }
    }
    /* big mistake... */
    sprintf(errorbuf, EMSG5, ptr);;
    XCError("parsing command string", errorbuf, Global);
}

char                           *
_XcBuildCommandString(Global)
XclipGlobalData *Global;
{
    char                           *comstr = _XgStrDup(Global->_xc_Data.argString);
    char                           *ptr = comstr;
    char                            text[512];

    bzero(text, sizeof(char) * 512);
    strcat(text, Global->_xc_Data.prog);
    strcat(text, " ");

    while (*ptr != '\0') {
        /* eat backquote part of backquoted (, [, ), or ]... */
        if (*ptr == '\\' &&
                (*(ptr + 1) == '(' || *(ptr + 1) == ')' ||
                 *(ptr + 1) == '[' || *(ptr + 1) == ']')) {
            ptr++;
            _XcCharCat(text, *ptr);
            ptr++;
            /* process flag or parm stuff */
        } else if (*ptr == '(') {
            char                           *insert;
            insert = DoParenText(&ptr,Global);
            if (insert != NULL) {
                strcat(text, insert);
                _XgFree(insert);
            }
            /* process optional stuff */
        } else if (*ptr == '[') {
            char                           *insert;
            insert = DoBracketText(&ptr,Global);
            if (insert != NULL) {
                strcat(text, insert);
                _XgFree(insert);
            }
            /* stow the rest */
        } else {
            _XcCharCat(text, *ptr);
            ptr++;
        }
    }
    return _XgStrDup(text);

}

_XcCharCat(s, c)
    char                           *s;
    char                            c;
{
    int                             end = strlen(s);
    s[end] = c;
    s[end + 1] = '\0';
}
