#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "usgs.h"

int get_buf (void)
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

int skip_file (void)
{
	int status;

	buffer = buf_start ;
	do {
		status = get_buf();
	} while(status);

	return 0;
}

int opentape (char *dev)
   {
	  tapefile = open(dev,0);
	  if(tapefile < 0)
	  {
		 perror(dev);
		 exit(1);
	  }

	return 0;
   }

int next_record (void)
{

  int the_rest;

  the_rest = RECORD_SIZE - (record_pos%RECORD_SIZE);
  if (buffer + the_rest >= buf_end) {
    the_rest -= buf_end - buffer;
    buffer = buf_start;
    if (get_buf() <= 0) {
      fprintf(stderr, "next_record: can't get more buffer\n");
      return 0;
    }
  }

  buffer += the_rest;
  record_pos = 0;

  return 0;
}
