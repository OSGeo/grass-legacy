
/* General routine to get tiger records.  Should work whether they
   contain <LF> or <CR> or neither or both.
   Returns: EOF for end of file; 0 for bad record_type requested;
            actual record length including possible <LF> and <CR>.
*/
#include "gis.h"
#define N_TIGERS 12

get_tiger_record(fp,type,buffer)
FILE *fp;
char type;
char *buffer;
{
static int record_size[N_TIGERS+1]=
          {0,228,208,111,58,52,76,74,36,98,52,44,46};
static int found_size[N_TIGERS+1]=
          {0,0,0,0,0,0,0,0,0,0,0,0,0};
static char record_type[N_TIGERS+1]=
          {'0','1','2','3','4','5','6','7','8','A','I','P','R'};

register int c,j;
int i,k,rec_size;
char msg[80];

  for (i=1; i<N_TIGERS+1; i++)
    if (type == record_type[i]) break;
  if (i > N_TIGERS) return (0);

  rec_size=record_size[i];

  /* skip <LF> and possible <CR>  and get first char */
  while (c=getc(fp)){
    if (c == EOF) return (EOF); 
    if (c == (int) type){
      buffer[0] = c;
      break;
    }
  }
  for (j=1; j<rec_size; j++)  /* get rest */
    if ( (buffer[j]=getc(fp)) < ' ')
      break;

  while (j<rec_size)    /* pad if necessary */
      buffer[j++] = ' ';

  buffer[rec_size] = '\0';    /* terminate with null */

  if (found_size[i]==0) {     /* determine actual rec size first time */
    found_size[i] = record_size[i];
    while (c=getc(fp)){
      if (c == EOF) break;
      if (c == (int) type){ungetc(c,fp); break;}
      ++ found_size[i];
      if ((found_size[i] - record_size[i]) > 3) {
        sprintf(msg,
          "Extra chars found in Tiger Type %c File--Check it.\n", type);
        G_warning(msg);
      }
    }
  }
  return (found_size[i]);
} /* end of get_tiger_record() */
