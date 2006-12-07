#include "global.h"


int
check_ready(void)
{
	int	retval;

	retval=0;

	if(!G_find_file("cell",iname,mapset)){
		G_warning("%s - not exists ",iname);
		retval=1;
	}

	return retval;
}

