/* %W% %G% */
#include "menu.h"
do_leave()
{
	P_writowin(PlanetW, "Exiting GIS system", 1, 1, 1);	
	sleep(2);
	P_menuexit(); 
	P_termexit(); 
	exit(0) ;
}
