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


/* advance to the next record in the buffer or get in the next
 * buffer full of data and them advance to the beginning of record.
 */
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
