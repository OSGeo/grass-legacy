#include <time.h>
#include "display.h"
#include "D.h"

int Dchoose(char *name)
{
	int stat ;

	if(stat = D_set_cur_wind(name))
		return(stat) ;
	else
		D_timestamp() ;

	return 0;
}
