#include <string.h>
#include "tape.h"
#define TM_EXTRACT "i.tape.tm.fast"
#define NONVERB " -q"

int tm_extract (void)
{
  char command[1024];
  char tmp[256];
  char bands[8];
  int j;
  int first = 1;

  for (j=0; j<THEMATIC_MAPPER_NBANDS; j++) {
    if (wantbands[j]) {
      if (first) {
        sprintf(tmp, "%d", j+1);
        first = 0;
      }
      else {
        sprintf(bands, ",%d", j+1);
        G_strip(bands);
        strcat(tmp, bands);
      }
    }
  }
  G_strip(tmp);    

  sprintf (command, "%s input=%s group=%s bands=%s ", TM_EXTRACT, inputname, 
	   groupname, tmp);

  if (!(verbose))
    strcat (command, NONVERB);

  if (rows) {
    sprintf (tmp, " rows=%d-%d ", firstrow, lastrow);
    strcat (command, tmp);
  }

  if (cols) {
    sprintf (tmp, " cols=%d-%d", firstcol, lastcol);
    strcat (command, tmp);
  }

  if (title != NULL) {
    strcat (command, " title=\"");
    strcat (command, title);         /* Here need to add a quote */
    strcat (command, "\"");
  }
  G_free(title);
  
  if (G_system(command))
    return 0;
  else
    return 1;
}
