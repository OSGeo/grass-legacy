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

	if(G_find_file("cell",oname,mapset)){
		if(overwr && !retval){
			G_remove("cell", oname);
		}else{
			fprintf(stderr, "\n** %s - already exists **\n", oname);
			retval=1;
		}
	}

	return(retval);
}

