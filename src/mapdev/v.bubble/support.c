#include "v.bubble.h"

/* Function "Date" provides today's date. */
int Date (char *today)
 {
  char month[4];
  char day[3];
  char year[5];
  char date[30];
  FILE *date_ptr;

  date_ptr = popen("date","r");
  fread(date,sizeof(date),1,date_ptr);
  *(month+0) = *(date+4);
  *(month+1) = *(date+5);
  *(month+2) = *(date+6);
  *(month+3) = '\0';
  if (*(date+8) == ' ')
    *(day+0) = '0';
  else
    *(day+0) = *(date+8);
  *(day+1) = *(date+9);
  *(day+2) = '\0';
  *(year+0) = *(date+24);
  *(year+1) = *(date+25);
  *(year+2) = *(date+26);
  *(year+3) = *(date+27);
  *(year+4) = '\0';
  pclose(date_ptr);
  sprintf(today,"%s %s, %s",month,day,year);
  return(0);
 }

