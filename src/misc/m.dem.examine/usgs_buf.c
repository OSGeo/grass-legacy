#include "usgs.h"

get_buf()
{
	int n_read;

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

opentape(dev)
 
   char *dev;
   {
	  tapefile = open(dev,0);
	  if(tapefile < 0)
	  {
		 perror(dev);
		 exit(1);
	  }
   }
