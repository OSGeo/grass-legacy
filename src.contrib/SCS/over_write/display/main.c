/*  %W%  %G%  */

/*****    The next define selects the target system.             ******/
/*****    This is necessary because the 386 and 3b2 machines     ******/
/*****    support different display devices.                     ******/

#define ATT_386 1 /* 1 for ATT_386 0 otherwise (may work for other 386) */

#include <stdio.h>
#include  "gis.h"

main() 
{
    char *name, *G__getenv() ;
    char command[80] ;

    G_gisinit("DISPLAY") ;

    if ((name = G__getenv("MONITOR")) == NULL)
    {
	    fprintf(stderr,"No graphics monitor has been selected for output.\n");
	    fprintf(stderr,"Please run \"monitor\" to select a graphics monitor.\n");
	    exit(-1);
    }
    else  /* which monitor */
    {
	if ( (strncmp(name,"DISx",2) == 0) ||
             (strncmp(name,"DUMB",2) == 0)   )
           {
	    sprintf( command, "%s/etc/d.display.pc ", G_gisbase());
  	    system( command) ; 
	    exit(0) ;
           }
        else if ( (strncmp(name,"EGATERM",7) == 0) ||
                  (strncmp(name,"VGATERM",7) == 0)   )
              {
	       sprintf( command, "%s/etc/d.display.eg ", G_gisbase());
  	       system( command) ; 
	       exit(0) ;
              }
#if ATT_386
        else if (strncmp(name,"DEGA",2) == 0)
              {
	       sprintf( command, "%s/etc/d.display.eg ", G_gisbase());
  	       system( command) ; 
	       exit(0) ;
              }
        else if (strncmp(name,"vdc600",2) == 0)
              {
	       sprintf( command, "%s/etc/d.display.eg ", G_gisbase());
  	       system( command) ; 
	       exit(0) ;
              }
        else if (strncmp(name,"DVGA",2) == 0)
              {
	       sprintf( command, "%s/etc/d.display.vg ", G_gisbase());
  	       system( command) ; 
	       exit(0) ;
              }
#endif

        else 
           {
           fprintf(stderr,"Display canNOT be run on monitor <%s>\n",name);
           fprintf(stderr,"NO such monitor is currently supported\n");
           exit (-1);
           }
    }
}
