#include "local_proto.h"


int
check_ready(void)
{
	FILE	*fp;
	int	retval;

	retval=0;

	if(!G_find_file("cell",iname,mapset)){
		fprintf(stderr, "\n** %s - not exists **\n", iname);
		retval=1;
	}

	if((fp=fopen(file,"r"))){
		fclose(fp);
		if(overwr && !retval){
			unlink(file);
		}else{
			fprintf(stderr, "\n** %s - file already exists **\n", 
									file);
			retval=1;
		}
	}

	return(retval);
}

