#include "xgen.h"

Environ *
IndexEnviron(ename)
	char *ename;
{
	Environ *e = xgenGD.toplevelEnv;

	while ( e ) {
		if ( !strcmp(e->name,ename)) return e;

		e = e->next;
	}
	return (Environ *)0;
}

Shell *
IndexShell(sname)
	char *sname;
{
	Environ *e = xgenGD.toplevelEnv;

	while ( e ) {
		Shell *s = e->shells;;

		while( s ) {
			if ( !strcmp(s->name,sname)) return s;

			s = s->next;
		}

		e = e->next;
	}
	return (Shell *)0;
}

Environ *
IndexEnvByShell(shell)
	Shell *shell;
{
	Environ *e = xgenGD.toplevelEnv;

	while ( e ) {
		Shell *s = e->shells;;

		while( s ) {
			if ( s == shell ) return e;

			s = s->next;
		}

		e = e->next;
	}
	return (Environ *)0;
}

Environ *
IndexEnvByObject(object)
	InterfaceObject *object;
{
	Environ *e = xgenGD.toplevelEnv;

	while ( e ) {
		Shell *s = e->shells;;

		while( s ) {
			InterfaceObject *o = s->objects;;

			while ( o ) {
				if ( o == object ) return e;

				o = o->next;
			}

			s = s->next;
		}

		e = e->next;
	}
	return (Environ *)0;
}

Environ *
IndexEnvByShellName(name)
	char *name;
{
	Environ *e = xgenGD.toplevelEnv;

	while ( e ) {
		Shell *s = e->shells;;

		while( s ) {
			if ( !strcmp(s->name,name)) return e;

			s = s->next;
		}

		e = e->next;
	}
	return (Environ *)0;
}

Environ *
IndexEnvByObjectName(name)
	char *name;
{
	Environ *e = xgenGD.toplevelEnv;

	while ( e ) {
		Shell *s = e->shells;;

		while( s ) {
			InterfaceObject *o = s->objects;;

			while ( o ) {
				if ( !strcmp(o->name,name)) return e;

				o = o->next;
			}

			s = s->next;
		}

		e = e->next;
	}
	return (Environ *)0;
}

Shell *
IndexShellByObject(object)
	InterfaceObject *object;
{
	Environ *e = xgenGD.toplevelEnv;

	while ( e ) {
		Shell *s = e->shells;;

		while( s ) {
			InterfaceObject *o = s->objects;;

			while ( o ) {
				if ( o == object ) return s;
				if ( o->type == PULLDOWN ) {
					InterfaceObject *o1 = o->objects;

					while ( o1 ) {
						if ( o1 == object ) return s;
						o1 = o1->next;
					}
				}

				o = o->next;
			}

			s = s->next;
		}

		e = e->next;
	}
	return (Shell *)0;
}

Shell *
IndexShellByObjectName(name)
	char *name;
{
	Environ *e = xgenGD.toplevelEnv;

	while ( e ) {
		Shell *s = e->shells;;

		while( s ) {
			InterfaceObject *o = s->objects;;

			while ( o ) {
				if ( !strcmp(o->name,name)) return s;

				o = o->next;
			}

			s = s->next;
		}

		e = e->next;
	}
	return (Shell *)0;
}

InterfaceObject *
IndexObjectByName(name)
	char *name;
{
	Environ *e = xgenGD.currentEnv;

	while ( e ) {
		Shell *s = e->shells;;

		while( s ) {
			InterfaceObject *o = s->objects;;

			while ( o ) {
				if ( !strcmp(o->name,name)) return o;

				o = o->next;
			}

			s = s->next;
		}
	
		e = e->next;
	}
	return (InterfaceObject *)0;
}

Boolean 
ShellInCurrentEnviron(s)
	Shell *s;
{
	Environ *e = IndexEnvByShell(s);

	if ( e == NULL ) return False;
	if ( e == xgenGD.currentEnv ) return True;
	return False;
}

Boolean
ShellHasPulldown(s)
	Shell *s;
{
	InterfaceObject *o = s->objects;;

	while ( o ) {
	    if ( o->type == PULLDOWN ) return True;
	    o = o->next;
	}

	return False;
}
