#include "usgs.h"

get_buf()
{
	int n_read;

{int i; for(i = 0; i < blocksize; i++)buffer[i] = 0;} /* shapiro - this is a bit dumb */
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

next_record()
{

  int the_rest;

  the_rest = RECORD_SIZE - (record_pos%RECORD_SIZE);
  if (buffer + the_rest >= buf_end) {
    the_rest -= buf_end - buffer;
    buffer = buf_start;
    if (get_buf() <= 0) {
      fprintf(stderr, "next_record: can't get more buffer\n");
      return NULL;
    }
  }

  buffer += the_rest;
  record_pos = 0;
}
