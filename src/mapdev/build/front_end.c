#include	<stdio.h>
#include	"gis.h"
#include	"dig_structs.h"

#define		DIG_DIR		"dig"
#define		PLUS_DIR	"dig_plus"
#define		ATT_DIR		"dig_att"

#define		PROG_NAME	"support.vect"

main()
{

	int   snap ;
	int   ram ;
	char  name[128] ;
	char  command[256] ;

	char  *mapset ;

	system( "clear") ;

	G_gisinit("IMPORT VECTOR FILES");

	mapset = G_ask_in_mapset( " BINARY VECTOR FILE TO CREATE SUPPORT FILES FOR ", name,
				DIG_DIR, "binary vector") ;
	if ( ! mapset)
		exit(0) ;
	
	if ( strcmp( mapset, G_mapset()) )
	{
		fprintf(stderr, " You're accessing a vector file in the mapset '%s'.\n", mapset) ;
		fprintf(stderr, " Please copy the vector file to your mapset\n") ;
		exit (-1);

	}

/*  check for existance of support directories  */
	G__make_mapset_element(PLUS_DIR) ;
	G__make_mapset_element(ATT_DIR) ;



/*  setup the args on the command line  */
	snap = G_yes( "\nDo you want to snap nodes to other nodes within a threshold ", 0) ;
	ram = 1 ;

/*
*  Usage:  support.vect  mapset  file_name snap=["yes", "no"] ram=["yes", "no"]
*    snap:    "no" to leave nodes as they are,
*             "yes" to snap nodes
*    ram:     "no" to read/write strictly from file
*             "yes" read everything into memory
*    thresh:  "no" use the default thresh
*             "yes" user wants to be asked for own threshold
*/
	sprintf( command, "%s/etc/%s  %s  %s  %s  %s %s ", G_gisbase(),
		PROG_NAME, mapset, name, snap ? "snap=yes": "snap=no",
		ram ? "ram=yes" : "ram=no", snap ? "thresh=yes" : "thresh=no") ;

	system( command) ;

	exit(0) ;

}


