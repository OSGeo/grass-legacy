#include "xgen.h"

void
ExpandVariable(resource)
	Resource *resource;
{
	char *savePtr = resource->varValue;
    char *tmp, *getenv();

	if ( *savePtr == '$' ) {
		savePtr++;
		tmp = getenv(savePtr);
		if ( NULL == tmp ) return;
		switch ( resource->type ) {
			case INTEGER:
				resource->val.ival = atoi(tmp);
			    break;	
			case REAL:
				resource->val.dval = atof(tmp);
			    break;	
			case BOOLEAN:
                resource->val.bval = 
                  (((!strcmp(tmp,"True")) ||
                  (!strcmp(tmp,"true")) ||
                  (!strcmp(tmp,"On"))   ||
                  (!strcmp(tmp,"on")))  ? True:False);
			    break;	
		}
	}
	return;
}
