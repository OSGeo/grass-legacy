
#include "graph.h"

R_panel_save(name, t, b, l, r)
	char *name ;
	int t, b, l, r ;
{
	int z ;
	int num ;

/* make sure this file can be written by anybody */
	num = umask(0);
	close(creat(name,0666));
	umask(num);

	_send_ident(PANEL_SAVE) ;
	_send_text(name) ;
	z = t ;
	_send_int(&z) ;
	z = b ;
	_send_int(&z) ;
	z = l ;
	_send_int(&z) ;
	z = r ;
	_send_int(&z) ;
	R_stabilize();
}

R_panel_restore(name)
	char *name ;
{
	_send_ident(PANEL_RESTORE) ;
	_send_text(name) ;
	R_stabilize();
}

R_panel_delete(name)
	char *name ;
{
	_send_ident(PANEL_DELETE) ;
	_send_text(name) ;

/* first make sure that driver is done with file */
	R_stabilize();
/* now delete the file, in case driver couldn't or didn't */
	unlink (name);
}
