/**********************************************************************
   update.c  - update object values
 **********************************************************************/
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

void
UpdateObjectValue(inres)
    Resource                       *inres;
{
    char                          **token, **Tokenize();
    Boolean                         save = verbose;
    int                             num;
    char expanded[8192];
    char *string;

    verbose = True;

    if ( inres->variable ) {
       string = inres->varValue;
    } else {
       string = inres->val.cval;
    }

    /*
     * parse the string and set the affected object value
     */
    token = Tokenize(string, ":");
    num = NumberOfTokens(token);
    if ( num == 3 ) { /* shell:object:value */
        Shell                          *s;
	InterfaceObject                *o;
	char *tptr;
	
	s = IndexShell(token[0]);
	if ( s == NULL ) {
	    sprintf(errorbuf,"no such shell \"%s\"", token[0]);
	    XgenWarning("update object value",
	       errorbuf);
	    verbose = save;
            return;
	}
	if ( (tptr = (char *)strrchr(token[1],'(')) != NULL ) {
	    *tptr = '\0';
	    o = IndexObjectByNameInShell(s, token[1]);
	    *tptr = '(';
	    tptr++; /* tptr points to table element */
	    if ( o->type != TABLE ) {
		sprintf(errorbuf,
		"object description \"%s\" in table format but is not a table", 
		token[1]);
		XgenWarning("update object value",
		   errorbuf);
		verbose = save;
		return;
	    }
	    if (NULL != strrchr(tptr, ',')) { /* we have a row, column spec */
		char **ptr;
		int r;
		int c;

		ptr = Tokenize(tptr,",");
		r = atoi(ptr[0]);
		c = atoi(ptr[1]);

		strcpy(expanded,token[2]);
		switch (ExpandString(expanded, 8192)) {
		case -1:
		    sprintf(errorbuf, 
			"a variable in string [%s] is undefined\n", token[2]);
		    XgenWarning("do update object", errorbuf);
		    return;
		case 0:
		    break;
		case 1:
		    sprintf(errorbuf, 
		        "string [%s] has been truncated\n", token[2]);
		    XgenWarning("do update object", errorbuf);
		    return;
		}
		XbaeMatrixSetCell((XbaeMatrixWidget) o->widget, r-1, c-1, expanded);
	    } else { /* we have an entire row spec */
		char **ptr;
		char **t;
		int num;
		int r;
		int c=0;
		int i;
		Resource *res;

		ptr = Tokenize(tptr,")");
		r = atoi(ptr[0]);

		strcpy(expanded,token[2]);
		switch (ExpandString(expanded, 8192)) {
		case -1:
		    sprintf(errorbuf, 
			"a variable in string [%s] is undefined\n", token[2]);
		    XgenWarning("do update object", errorbuf);
		    return;
		case 0:
		    break;
		case 1:
		    sprintf(errorbuf, 
		        "string [%s] has been truncated\n", token[2]);
		    XgenWarning("do update object", errorbuf);
		    return;
		}
		if (NULL != 
		    (res = IndexResource((char *) o, OBJECT, "columns"))) {
		    if (res->variable)
			ExpandVariable(res, (char *) o, OBJECT);
		    c = res->val.ival;
		} else {
		    XgenFatalError("clear table", "no column count");
		}
		
		t = Tokenize(expanded,",");
		num = NumberOfTokens(t);
		if ( num != c ) {
		    sprintf(errorbuf,
			"not enough elements in \"%s\" for table row update\n",
			token[2]);
		    XgenWarning("update object value",errorbuf);
		    verbose = save;
		    return;
		}
		for (i = 0; i < c; i++) {
		    XbaeMatrixSetCell((XbaeMatrixWidget) o->widget, r-1, i, t[i]);
		}
	    }
	    verbose = save;
	    return;
	} else {
	    o = IndexObjectByNameInShell(s, token[1]);
	}
	if ( o == NULL ) {
	    sprintf(errorbuf,"object \"%s\" not in shell \"%s\"", 
               token[1], token[0]);
	    XgenWarning("update object value",
	       errorbuf);
	    verbose = save;
            return;
	}
	switch(o->type) {
	    case TABLE:
                {
		    Resource                       *tres;
		    int                             rows=0, columns=0, r, c, i;
		    char **t;
		    int num;

		    if (NULL != 
                        (tres = IndexResource((char *) o, OBJECT, "rows"))) {
			if (tres->variable)
			    ExpandVariable(tres, (char *) o, OBJECT);
			rows = tres->val.ival;
		    } else
			XgenFatalError("clear table", "no row count");
		    if (NULL != 
                        (tres = IndexResource((char *) o, OBJECT, "columns"))) {
			if (tres->variable)
			    ExpandVariable(tres, (char *) o, OBJECT);
			columns = tres->val.ival;
		    } else {
			XgenFatalError("clear table", "no column count");
                    }

		    strcpy(expanded,token[2]);
		    switch (ExpandString(expanded, 8192)) {
		    case -1:
			sprintf(errorbuf, 
			    "a variable in string [%s] is undefined\n", 
                            token[2]);
			XgenWarning("do update object", errorbuf);
			return;
		    case 0:
			break;
		    case 1:
			sprintf(errorbuf, 
			    "string [%s] has been truncated\n", token[2]);
			XgenWarning("do update object", errorbuf);
			return;
		    }

		    t = Tokenize(expanded,",");
		    num = NumberOfTokens(t);
		    if ( num != columns*rows ) {
			sprintf(errorbuf,
			    "not enough elements in \"%s\" for table update\n",
			    token[2]);
			XgenWarning("update object value",errorbuf);
			verbose = save;
			return;
		    }

                    i = 0;
		    for (r = 0; r < rows; r++)
			for (c = 0; c < columns; c++)
			    XbaeMatrixSetCell((XbaeMatrixWidget) o->widget, 
                                             r, c, t[i++]);
		}
		break;
	    case LIST:
	    case TOGGLE:
		XgenWarning("update object value",
		   "lists and toggles cannot be updated");
		break;
	    default:
		DoUpdateWidget(o, token[2]);
	}
    } else {
        XgenWarning("update object value",
           "string must be of form shell:object:value");
    }

    verbose = save;
}

void
DoUpdateWidget(o, s)
    InterfaceObject *o;
    char *s;
{
    char expanded[8192];
    XmString xms;
    Resource *r;

    strcpy(expanded, s);
    switch (ExpandString(expanded, 8192)) {
    case -1:
	sprintf(errorbuf, "a variable in string [%s] is undefined\n", s);
	XgenWarning("do update object", errorbuf);
	return;
    case 0:
	break;
    case 1:
	sprintf(errorbuf, "string [%s] has been truncated\n", s);
	XgenWarning("do update object", errorbuf);
	return;
    }

    switch (o->type) {
    case LABEL:
    case PUSHBUTTON:
        xms = XmStringCreateLtoR(expanded,SDC);
        XtVaSetValues(o->widget, XmNlabelString, xms, NULL);
	break;
    case TEXTENTRY:
    case MULTILINE:
        XtVaSetValues(o->widget, XmNvalue, expanded, NULL);
	break;
    case SLIDER:
        if (NULL != (r = IndexResource((char *) o, OBJECT, "decimalpoints"))) {
	    if ( CheckType(expanded, Real) ) {
		XmScaleSetValue(o->widget, 
#ifdef SVR4
                    (int)(strtod(expanded, NULL)*ten(r->val.ival)));
#else
#ifdef BSD
                    (int)(atof(expanded)*ten(r->val.ival)));
#else
                    (int)(strtod(expanded, NULL)*ten(r->val.ival)));
#endif
#endif
	    } else {
		sprintf(errorbuf, "string [%s] must be a real number\n", s);
		XgenWarning("do update object", errorbuf);
	    }
        } else {
	    if ( CheckType(expanded, Int) ) {
		XmScaleSetValue(o->widget, atoi(expanded));
	    } else {
		sprintf(errorbuf, "string [%s] must be an integer\n", s);
		XgenWarning("do update object", errorbuf);
	    }
        }
	break;
    }
}
