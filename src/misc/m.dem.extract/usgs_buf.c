#include "usgs.h"

get_buf()
{
  int i,j, n, n_read, count;
  static char *str = NULL;

  if (str==NULL) {str = G_malloc(blocksize + 1024); 
		  /*fprintf(stdout, "\n buf is null \n"); */
		  }
  if(buffer != buf_start)
    printf("memory overwrite, buffer= %d buf_start= %d\n",buffer,buf_start);

  if((n_read = read(tapefile,buffer,blocksize)) < 0){
    unlink(inf);
    unlink(of);
    G_fatal_error("error reading tape");
  }
  buffer[n_read] = '\0';
  /* Checking for the newlines and returns */
  /* ususally when the data is extracted from tape to a disk file
     there are some newline and return chars thrown in. so be4
     reading the buffer we have to throw away these chars */
  count = 0;
  for(i=0;buffer[i]&&(i<n_read);i++)
  {
    if ((buffer[i] == '\n')||(buffer[i]=='\r'))
    {
      count++;
      strcpy((buffer+i),(buffer+i+1));
      /* throw away this char */
      i--;
    }
  }
  if(count>0)
  {
       if(n_read==blocksize)
       /* read count more characters to compensate for the thrown away '\n's */
       {
          if((n = read(tapefile,str,count)) < 0){
             G_fatal_error("error reading tape");
             unlink(inf);
             unlink(of);
	     }
          str[n] = '\0';
	  strcpy((buffer + n_read - count),str);
          if(n<count) n_read = n_read - (count - n);
       }
       else
	  n_read-=count;
  }

  buffer[n_read] = '\0';
  filestat = n_read;

/*DEBUG:*/
/*
  fprintf(stdout, "%s",buffer); 
  fprintf(stdout, "blocksize: %d, n_read: %d\n", blocksize, n_read); 
*/

  buf_end = buf_start + n_read;
  return(n_read);
}

/*
skip_file()
{
  int status;

  buffer = buf_start ;
  do {
    status = get_buf();
  } while(status);
}

*/
skip_file()
{
    int i, roww, elev;
    int records_in_prof;

/*  read first profile from file          */

    get_profile();

    while((bas_e <= P_cols))
        {


        /*  continue reading remainder of last profile */
                for(roww = 0;roww < rows;roww++)
                  buffer +=  get_int(&elev);
        /*
	records_in_prof = rows*6/1024;
	for(i=1;i<=records_in_prof;i++) 
	  {
	    next_record();
	    record_pos = record_pos + 1024;
          }
        */

        /*  now ready to get next profile */
		if(bas_e < P_cols)
		  {
                       if(!(get_profile()))
                               return(0);
                       /* prof_processed++; */
                  }
                 else break;
        }


/*  read rest of file */

/* here we need to read type C record and the rest of type B records) */

        if(C_record==1)
	   {
              next_record();
	      record_pos = record_pos + 1024;
	   /* 
	    int i, accur;
	    for(i=1;i<=10;i++)
		 buffer += get_int(&accur);
            */
           }
        return(1);
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
    if (get_buf() <= 0) 
   {
    /* in case of tape it might be just end of file so try again */
/*      if(get_buf()<=0) */
          if(get_buf()<=0)
          {
      	    fprintf(stdout,"----------------------------------------------\n");
	    fprintf(stdout, " end of records\n");
            return /* NULL*/ (0); 
          }
    }
  }

  buffer += the_rest;
  record_pos = 0;
  return 1;
}
