/* %W% %G% */
#include "usgs.h"

get_buf()
{
	int n_read;

	if(buffer != buf_start)
	printf("memory overwrite, buffer= %d buf_start= %d\n",buffer,buf_start);

	if((n_read = read(tapefile,buffer,blocksize)) < 0){
		G_fatal_error("error reading tape");
		exit(1);
	}
	buffer[n_read] = 0;
	filestat = n_read;
	return(n_read);
}

skip_file()
{
	int status;

	buffer = buf_start ;
	do {
		status = get_buf();
	} while(status);
}
