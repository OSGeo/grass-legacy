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

	if(G_find_file("cell",oname,mapset)){
		if(overwr && !retval){
			G_remove("cell", oname);
		}else{
			fprintf(stderr, "\n** %s - already exists **\n", oname);
			exit(1);
		}
	}

	return(retval);
}

