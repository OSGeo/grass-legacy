#include "xgen.h"
#include "grammar.h"
#include "resource.h"
#define END(v) (v-1 + sizeof v / sizeof v[0])


char *
ResourceString(resource)
	int resource;
{
	struct _resourceTable *low = resourceTable, *high = END(resourceTable);

	while( low <= high ) {
		if ( low->index == resource ) return low->name;
		low++;
	}
	return(NULL);
}

int
ResourceIndex(resource)
	char * resource;
{
	struct _resourceTable *low = resourceTable, *high = END(resourceTable);

	while( low <= high ) {
		if ( !strcmp(low->name,resource) ) return low->index;
		low++;
	}
	return(NULL);
}

int
ResourceDataType(resource)
	int resource;
{
	struct _resourceTable *low = resourceTable, *high = END(resourceTable);

	while( low <= high ) {
		if ( low->index == resource ) return low->type;
		low++;
	}
	return(UNKNOWN);
}

unsigned int
ResourceValid(resource)
	int resource;
{
    struct _resourceTable *low = resourceTable, *high = END(resourceTable);

	while( low <= high ) {
		if ( low->index == resource ) return low->valid;
		low++;
	}
	return(1L<<0);
}

unsigned int
ShellObjectValid(shell)
	int shell;
{
    struct _shellTable *low = shellTable, *high = END(shellTable);

	while( low <= high ) {
		if ( low->index == shell ) return low->valid;
		low++;
	}
	return(1L<<0);
}

char *
ShellString(shell)
	int shell;
{
    struct _shellTable *low = shellTable, *high = END(shellTable);

	while( low <= high ) {
		if ( low->index == shell ) return low->name;
		low++;
	}
	return(NULL);
}

char *
ObjectString(object)
	int object;
{
    struct _objectTable *low = objectTable, *high = END(objectTable);

	while( low <= high ) {
		if ( low->index == object ) return low->name;
		low++;
	}
	return(NULL);
}

Resource *
IndexResource(cptr,type,rname)
	caddr_t *cptr;
	int type;
	char *rname;
{
	Resource *p;

	switch(type) {
		case ENVIRONMENT:
			{
			Environ *ptr = (Environ *)cptr;

			for ( p = ptr->resources; p != NULL; p = p->next )
				if ( !strcmp(p->name,rname))
					return p;
			}
			break;
		case SHELL:
			{
			Shell *ptr = (Shell *)cptr;

			for ( p = ptr->resources; p != NULL; p = p->next )
				if ( !strcmp(p->name,rname))
					return p;
			}
			break;
		case OBJECT:
			{
			InterfaceObject *ptr = (InterfaceObject *)cptr;

			for ( p = ptr->resources; p != NULL; p = p->next )
				if ( !strcmp(p->name,rname))
					return p;
			}
			break;
	}
	return NULL;
}
