#include "local_proto.h"


int
check_ready(void)
{
	int	retval;

	retval=0;

	if(!G_find_file("cell",iname,mapset)){
		fprintf(stderr,"\n** %s - not exists **\n",iname);
		retval=1;
	}

	return retval;
}

