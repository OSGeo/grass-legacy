#include	<stdio.h>
#include	"gis.h"
#include	"dig_structs.h"

#define		DIG_DIR		"dig"
#define		PLUS_DIR	"dig_plus"
#define		ATT_DIR		"dig_att"

#define		PROG_NAME	"v.build"

main(argc,argv)
	char **argv ;
{

	int   snap ;
	int   ram ;
	char  name[128] ;
	char  command[256] ;

	char  *mapset ;

	system( "clear") ;

	G_gisinit(argv[0]) ;

	mapset = G_ask_in_mapset( "\n\n BINARY VECTOR FILE TO CREATE SUPPORT FILES FOR ", name,
				DIG_DIR, "binary vector") ;
	if ( ! mapset)
		exit(0) ;
	
	if ( strcmp( mapset, G_mapset()) )
	{
		fprintf(stderr, "\n\n You are accessing a vector file in the mapset '%s'.\n", mapset) ;
		fprintf(stderr, " Please copy the vector file to your mapset before proceeding.\n") ;
		exit (-1);

	}

/*  check for existance of support directories  */
/* unnecessary, done by real v.build:
	G__make_mapset_element(PLUS_DIR) ;
	G__make_mapset_element(ATT_DIR) ;
*/


/*  setup the args on the command line  */
	snap = G_yes( "\nDO YOU WANT TO SNAP NODES TO OTHER NODES WITHIN A THRESHOLD ", 0) ;
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
	sprintf( command, "v.build map=%s %s", 
		name, snap ? "-st": "");

	system( command) ;

	exit(0) ;

}


