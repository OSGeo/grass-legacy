#include <unistd.h>
#include "menu.h"

int do_leave (void)
{
	P_writowin(PlanetW, "Exiting GRASS help system", 1, 1, 1);	
	sleep(1);
	P_menuexit(); 
	P_termexit(0); 
	exit(0) ;
}
