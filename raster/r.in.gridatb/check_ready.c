#include "local_proto.h"


int
check_ready(void)
{
	FILE	*fp;
	int	retval;

	retval=0;

	if(!(fp=fopen(file,"r"))){
		fprintf(stderr, "\n** %s - file not exists **\n", file);
		retval=1;
	}else{
		fclose(fp);
	}

	return(retval);
}

