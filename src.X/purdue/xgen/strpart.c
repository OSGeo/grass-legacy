#include "xgen.h"

char *strpart(s,b,e)
	char *s;
	int b, e;
{
	char *ret;

	if ( !s || e <= b || e <= 0 || b <= 0 || e > strlen(s)) return(NULL);
	ret = XtMalloc(e - b + 2);
	strncpy(ret,s + b - 1,e - b + 1);
	ret[e - b + 1] = '\0';
	return(ret);
}
