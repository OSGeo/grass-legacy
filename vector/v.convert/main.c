/* *****************************************************************
 * *
 * * MODULE:       v.convert
 * * 
 * * AUTHOR(S):    Radim Blazek - Radim.Blazek@dhv.cz
 * *               
 * * PURPOSE:      Convert GRASS vector files versions:
 * *               from 3 or 4 to 5.0
 * *               or
 * *               from 5.0 to 4.10
 * *               
 * * COPYRIGHT:    (C) 2001 by the GRASS Development Team
 * *
 * *               This program is free software under the 
 * *               GNU General Public License (>=v2). 
 * *               Read the file COPYING that comes with GRASS
 * *               for details.
 * *
 * ****************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include "gis.h"
#include "Vect.h"
#include "conv.h"
#include "local_proto.h"

int 
main (int argc, char *argv[])
{
    struct Option *opt_in, *opt_out, *opt_ver, *opt_end; 
    int    endian;
    struct GModule *module;
    
    module = G_define_module();
    module->description = "Converts between GRASS vector versions.";
    
    /* input vector map */
    opt_in = G_define_option();
    opt_in->key         = "input";
    opt_in->type        = TYPE_STRING ;
    opt_in->required    = YES ;
    opt_in->multiple    = NO ;
    opt_in->gisprompt   = "vector" ;
    opt_in->description = "input vector map";  
    
    /* output vector map */
    opt_out = G_define_option();
    opt_out->key         = "output";
    opt_out->type        = TYPE_STRING ;
    opt_out->required    = YES ;
    opt_out->multiple    = NO ;
    opt_out->gisprompt   = "vector" ;
    opt_out->description = "output vector map";      

    /* output version */
    opt_ver = G_define_option();
    opt_ver->key          = "version";
    opt_ver->type         = TYPE_INTEGER;
    opt_ver->required     = NO;
    opt_ver->multiple     = NO;
    opt_ver->options      = "4,5";
    opt_ver->description  = "version of output vector map";
    opt_ver->answer = "5"; 

    /* endian of input vector map */
    opt_end = G_define_option();
    opt_end->key         = "endian";
    opt_end->type        = TYPE_STRING ;
    opt_end->required    = NO;
    opt_end->multiple    = NO;
    opt_end->options      = "little,big";    
    opt_end->description = "endian of input vector map";      
    opt_end->answer = "big"; 
    
    G_gisinit(argv[0]);
    
    if (G_parser(argc,argv))
        exit(-1);   


    /* Numbers in portable format files are saved as big endians */
    if ( opt_end->answer[0] == 'l' )  
	endian = ENDIAN_LITTLE;
    else	
	endian = ENDIAN_BIG;

    if ( opt_ver->answer[0] == '5' )  
        old2new (opt_in->answer, opt_out->answer, endian);
    else {
        fprintf(stdout,"Not yet implemented.\n");
        /* new2old (opt_in->answer, opt_out->answer); */
    }
	    
    exit(0);
}




