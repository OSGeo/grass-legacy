/**************************************************************************/
/***				interface()				***/
/***    Function to get input from user and check files can be opened	***/
/*** 									***/
/***      Jo Wood, Department of Geography, V1.2, 7th February 1992	***/
/**************************************************************************/

#include "ntf_in.h"

interface(argc,argv) 

    int	     argc;		/* Number of command line arguments.	*/
    char    *argv[];		/* Contents of command line arguments.	*/

{
    /*------------------------------------------------------------------*/
    /*                          INITIALISE				*/
    /*------------------------------------------------------------------*/ 

    struct Option *ntf_in;	/* Pointer to a structure holding	*/
				/* all the text to describe each option.*/
				/* It also stores the user's input.	*/

    struct Option *file_out;
			

    struct Flag	  *out_log;	/* Conversion log flag.			*/

/* ---- Ignore ---- 
    struct Flag	  *nde_inc;	Include node flag.		
 ---- End Ignore ---*/

    G_gisinit (argv[0]);	/* This GRASS library function MUST be 
				/* called to initialise the program.	*/

    /*------------------------------------------------------------------*/
    /*                       SET PARSER OPTIONS				*/
    /*------------------------------------------------------------------*/

    /* Each option needs a 'key' (short description), a 'description` 	*/
    /* (a longer one), a 'type' (eg intiger, or string), and an 	*/
    /* indication whether manditory or not.				*/

    ntf_in  		  = G_define_option();
    ntf_in->key	  	  = "ntf";
    ntf_in->description   = "NTF file to read";
    ntf_in->type	  = TYPE_STRING;
    ntf_in->required	  = YES;

    file_out 		  = G_define_option();	
    file_out->key	  = "out";
    file_out->description = "Output file";
    file_out->type	  = TYPE_STRING;
    file_out->required	  = NO;
    file_out->answer	  ="No output file";

    out_log		  = G_define_flag();
    out_log->key	  ='l';
    out_log->description  ="Sends an NTF conversion log to standard output";

/* --- Ignore --- 
    nde_inc		  = G_define_flag();
    nde_inc->key	  ='n';
    nde_inc->description  ="Include transfer of nodes into vector";
 --- End Ignore --- */

    if (G_parser(argc,argv))	/* Actually performs the prompting for	*/
	exit(-1);		/* keyboard input. Returns 0 on sucess.	*/

				/* Transfer command line answers 	*/
				/* to variables.			*/
    ntf_in_name = ntf_in->answer;	
    file_out_name = file_out->answer;

    conversion_log = out_log->answer;

/* --- Ignore ---
    nodes = nde_inc->answer;
 --- End Ignore --- */

    if (strcmp(file_out_name,"No output file"))
	outfile = TRUE;
    else
	outfile = FALSE;

}
