#include "raster.h"
#include "graph.h"
static int first = 1 ;
static int t, b, l, r ;
static int getall(void);

int R_screen_left()
{
	if(first) getall() ;
	return(l) ;
}

int R_screen_rite()
{
	if(first) getall() ;
	return(r) ;
}

int R_screen_bot()
{
	if(first) getall() ;
	return(b) ;
}

int R_screen_top()
{
	if(first) getall() ;
	return(t) ;
}

static int getall()
{
	_send_ident(SCREEN_TOP) ;
	_get_int(&t) ;
	_send_ident(SCREEN_BOT) ;
	_get_int(&b) ;
	_send_ident(SCREEN_LEFT) ;
	_get_int(&l) ;
	_send_ident(SCREEN_RITE) ;
	_get_int(&r) ;
	first = 0 ;

	return 0;
}
