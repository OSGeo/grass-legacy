/* %W% %G% */
/* main.vect    1.0   4/01/91
/* created from fortran program RLG,
* to direct DOT MAP processes                       
*
*  ------Rev 3.+ arguements --------------------------------------
*    Input arguements:
*             Vdot         in=      input vector file to read
*                          out=     output site_list file to create
*                          dot=     label to dot count file
*                                                                       
*
*  ------Rev 4.+ arguements --------------------------------------
*    Input arguements:
*             v.random map=  vector file to read
*                      site= output site_list file
*                      dot=  label to dot count file
*
*    flags:
*         -n      : use category numbers, NOT names
*         -s      : determine optimum dot size, no dot processing
*         -v      : verbose mode
*
*/  
                                                                    
#include <stdio.h>
#include <unistd.h>
#include <ctype.h>
#include  "gis.h"
#include  "local_proto.h"
#define MAIN

int 
main (int argc, char *argv[])

{
    int names=1, talk=0;
    char buffr[100], dotfile[100], *mapset;
    char input[128], output[128] ;
	struct GModule *module;
    struct Option *siteopt, *mapopt, *dotopt;
    struct Flag *n_flag, *s_flag, *v_flag;

    G_gisinit (argv[0]);
     
	module = G_define_module();
	module->description =
		"Creates a GRASS site_lists file of randomly placed "
		"symbols (sites) within a GRASS vector area.";

		 /* set up the options and flags for the command line parser */

    mapopt = G_define_option();
    mapopt->key             = "map";
    mapopt->type            =  TYPE_STRING;
    mapopt->required        =  YES;
    mapopt->description     = "input vector file name";

    siteopt = G_define_option();
    siteopt->key             = "site";
    siteopt->type            =  TYPE_STRING;
    siteopt->required        =  NO;
    siteopt->description     = "output site_list file name";

    dotopt = G_define_option();
    dotopt->key             = "dot";
    dotopt->type            =  TYPE_STRING;
    dotopt->required        =  YES;
    dotopt->description     = "file name containing labels and dot counts";
    
    n_flag = G_define_flag();
    n_flag->key              = 'n';
    n_flag->description      = "Use category numbers NOT names ";

    s_flag = G_define_flag();
    s_flag->key              = 's';
    s_flag->description      = "Determine optimum dot size";

    v_flag = G_define_flag();
    v_flag->key              = 'v';
    v_flag->description      = "Verbose mode";


    if (G_parser (argc, argv))
		    exit (-1);
		     
           /* start checking options and flags */
    if (n_flag->answer) names = 0;
    if (v_flag->answer) talk = 1;
    if (s_flag->answer) 
       {    /* set output site_list file to NULL */
       output[0] = '\0';
       }
    else       /* set output site_list file name */
       {
       if (siteopt->answer) sprintf(output,"%s",siteopt->answer);
       else
	  {
	  sprintf(buffr,"\nERROR: An output file is specified");
	  G_fatal_error(buffr) ;
	  }
       }
									  
           /* check for input vector file name and mapset */
    sprintf(input,"%s",mapopt->answer);

    mapset = G_find_vector (input, "") ;
    if (mapset == NULL)
	{
		sprintf(buffr,"Vector file [%s] not available in search list",
		    input);
		G_fatal_error(buffr) ;
	}

           /* check for input dot file name and mapset */
    sprintf(dotfile,"%s",dotopt->answer);
    if (access(dotfile,0) != 0)
	{
		sprintf(buffr,"dot file [%s] not found", dotfile);
		G_fatal_error(buffr) ;
	}
     

    do_dots(input,output,dotfile,names,talk); 

    exit(0);
}
