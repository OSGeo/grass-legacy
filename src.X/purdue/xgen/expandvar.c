#include "xgen.h"

void
ExpandVariable(resource)
	Resource *resource;
{
	char *savePtr = resource->varValue;
    char *tmp, *getenv();

	if ( resource->type == STRING && resource->variable ) {
		char expanded[1024];

		strcpy(expanded,resource->varValue);
		switch(ExpandString(expanded,1024)) {
			case -1:
				sprintf(errorbuf,
                    "a variable in string [%s] is undefined\n",
                    resource->val.cval);
                XgenWarning("expand variables",errorbuf);
				break;
			case 0:
				if ( resource->val.cval ) XtFree(resource->val.cval);
				resource->val.cval = SaveString(expanded);
				break;
			case 1:
				sprintf(errorbuf,"string [%s] has been truncated\n",
                    resource->val.cval);
                XgenWarning("expand variables",errorbuf);
				break;
		}
		return;
	}
	if ( *savePtr == '$' ) {
		int ival;
		double dval;

		savePtr++;
		tmp = getenv(savePtr);
		if ( NULL == tmp ) return;
		switch ( resource->type ) {
			case INTEGER:
				if ( CheckType(tmp,Integer))
				    resource->val.ival = atoi(tmp);
				else 
					XgenFatalError("expand variable",errorbuf);
			    break;	
			case REAL:
				if ( CheckType(tmp,Real))
				    resource->val.dval = strtod(tmp,NULL);
				else 
					XgenFatalError("expand variable",errorbuf);
			    break;	
			case BOOLEAN:
				if ( CheckType(tmp,OnOff))
                    resource->val.bval = 
                      (((!strcmp(tmp,"True")) ||
                      (!strcmp(tmp,"true")) ||
                      (!strcmp(tmp,"On"))   ||
                      (!strcmp(tmp,"on")))  ? True:False);
				else 
					XgenFatalError("expand variable",errorbuf);
			    break;	
		}
	}
	return;
}
