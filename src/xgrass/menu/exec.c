static char rcsid[] = "@(#)XGRASS $Id: exec.c,v 0.0.0.1 1992/05/05 14:58:29 kurt Exp kurt $";
/*
 * File:
 *
 * Desc:
 *
 * Auth:
 *
 * Date:
 *
 * Modification History:
 *
 *
 */

#include "xgrass.h"

XgrassExec(argc,argv)
int argc;
char **argv;
{
   int i;

   fprintf(stderr,"xgrass_exec ");
   for ( i = 0; i < argc;  i++ ) 
       fprintf(stderr,"%s ",argv[i]);
   fprintf(stderr,"\n");

}

XgrassBackgoundExec(argc,argv)
int argc;
char **argv;
{
   int i;

   fprintf(stderr,"xgrass_bg_exec ");
   for ( i = 0; i < argc;  i++ ) 
       fprintf(stderr,"%s ",argv[i]);
   fprintf(stderr,"\n");
}
