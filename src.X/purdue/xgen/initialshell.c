#include "xgen.h"
#include "grammar.h"

InitialShell(eptr)
	Environ *eptr;
{
	/* try to retrieve the initialshells resource */
	Resource *ishells = IndexResource(eptr,ENVIRONMENT,"initialshells");
	Resource *resource;
	char *ilist = NULL;
	char *token;
	Boolean first = False;

	/* if couldn't find them set the initialshells to the first shell */
	if ( ishells == NULL ) {
		ilist = eptr->shells->name;
		AddResource(AllocResource(),eptr,ENVIRONMENT,ilist,InitialShells);
	} else
		ilist = ishells->val.cval;

	/* parse the lists (if more than one name occurs) and turn on flags */
	/* initial shells */
	do {
		if ( !first ) {
			token = strtok(ilist," ");
			first = True;
		} else 
			token = strtok(NULL," ");
		if ( token ) {
			Shell *s;

			if ((s = IndexShell(token)) == NULL) {

				sprintf(errorbuf,"non-existant shell %s",token);
				XgenFatalError("processing initial shells",errorbuf);
			}
			s->initial = True;
		} 
	} while(token != NULL );
	ilist = NULL;

}
