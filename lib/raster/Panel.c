#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include "raster.h"
#include "graph.h"

int R_panel_save(char *name, int t,int b,int l,int r)
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

	return 0;
}

int R_panel_restore(char *name )
{
	_send_ident(PANEL_RESTORE) ;
	_send_text(name) ;
	R_stabilize();

	return 0;
}

int R_panel_delete(char *name )
{
	_send_ident(PANEL_DELETE) ;
	_send_text(name) ;

/* first make sure that driver is done with file */
	R_stabilize();
/* now delete the file, in case driver couldn't or didn't */
	unlink (name);

	return 0;
}
