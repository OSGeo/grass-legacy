#include "xgen.h"

static char * Gstore () ;

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
			case STRING:
				resource->val.cval = Gstore (tmp);
			    break;
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

static char *
Gstore (s) 
    char *s;
{
    char *buf;
    char *malloc();

    buf = malloc (strlen(s) + 1);
    if (buf == NULL)
	fprintf (stderr, "Malloc out of memory\n"), exit (0);
    strcpy (buf, s);
    return buf;
}
