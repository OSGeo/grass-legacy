#include "xgen.h"

ParsePath(p,part)
char *p; /* the path to parse */
FileParts * part;
{
	int plen = strlen(p) + 1, nlen, elen;

	if ( strrchr(p,'/') == NULL ) {
		nlen = plen; 
		part->n = (char *)XtMalloc(plen);
		part->n = p; 
		part->d = (char *)XtMalloc(3); 
		part->d = "./";
	} else {
	    nlen = strlen(strrchr(p,'/'));
		if ( nlen == 1 ) { 
			part->n == NULL;
			part->b == NULL;
			part->e == NULL;
		} else {
			part->n = (char *)XtMalloc(nlen);
			part->n = strrchr(p,'/') + 1;
		}
		part->d = (char *)XtMalloc(plen - nlen + 1);
		(void)strcpy(part->d,p);
		part->d[plen - nlen] = '\0';
		if ( part->n == NULL ) return;
	}
	if ( !strcmp(part->n,"..") ) {
		part->b = (char *)XtMalloc(3);
		part->b = "..";
		part->e = NULL;
		return;
	}
	if ( strrchr(part->n,'.') == NULL ) {
		elen = 0;
		part->e = NULL;
	} else {
		elen = strlen(strrchr(part->n,'.')) + 1;
		part->e = (char *)XtMalloc(elen);
		part->e = strrchr(part->n,'.');
	}
	if ( nlen == elen ) {
		part->b = NULL;
	} else {
		part->b = (char *)XtMalloc(nlen - elen + 1);
		(void)strcpy(part->b,part->n);
		part->b[nlen - elen] = '\0';
	}
}
