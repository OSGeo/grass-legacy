/* %W% %G% */
/* main.vect    1.0   4/01/91
*
*    Input arguements:
*             v.random map=  vector file to read
*                      stat= vector stats file dig_stats
*

*    flags:
*         -v      : verbose mode
*
*/  
                                                                    
#include <stdio.h>
#include <ctype.h>
#include  "gis.h"
#define MAIN

main (argc,argv)
int argc;
char *argv[];

{
    int names=1, talk=0, out=0;
    char buffr[100], dotfile[100], *mapset;
    char input[128];
    struct Option *mapopt;
    struct Flag *v_flag, *n_flag, *s_flag;

    G_gisinit (argv[0]);
     
		 /* set up the options and flags for the command line parser */

    mapopt = G_define_option();
    mapopt->key             = "map";
    mapopt->type            =  TYPE_STRING;
    mapopt->required        =  YES;
    mapopt->description     = "input vector file name";

    v_flag = G_define_flag();
    v_flag->key              = 'v';
    v_flag->description      = "Verbose mode";
 
 /*
	n_flag = G_define_flag();
	n_flag->key              = 'n';
	n_flag->description      = "Use category numbers NOT names ";
*/
			  
 
	s_flag = G_define_flag();
	s_flag->key              = 's';
	s_flag->description      = "Create a dig_stats file ";
			  

    if (G_parser (argc, argv))
		    exit (-1);
		     
           /* start checking options and flags */
/*
if (n_flag->answer) names = 0;
*/
    if (v_flag->answer) talk = 1;
									  
           /* check for input vector file name and mapset */
    sprintf(input,"%s",mapopt->answer);

    mapset = G_find_vector (input, "") ;
    if (mapset == NULL)
	{
		sprintf(buffr,"Vector file [%s] not available in search list",
		    input);
		G_fatal_error(buffr) ;
	} 
	if (s_flag->answer) out = 1;

     
    do_stats(input,names,talk,out); 

    exit(0);
}
