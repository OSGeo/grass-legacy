#include "xgen.h"

UniqueEnvName(name)
	char *name;
{
	Environ *eptr = xgenGD.toplevelEnv;	

	while ( eptr != (Environ *)0 ) {
		if ( !strcmp(name,eptr->name) ) return(0);
		eptr = eptr->next;
	}
	return(1);
}

UniqueShellName(name)
	char *name;
{
	Environ *eptr = xgenGD.toplevelEnv;	

	while ( eptr != (Environ *)0 ) {
		Shell *sptr = eptr->shells;

		while ( sptr != (Shell *)0 ) {
			if ( !strcmp(name,sptr->name) ) return(0);
			sptr= sptr->next;
		}
		eptr = eptr->next;
	}
	return(1);
}
