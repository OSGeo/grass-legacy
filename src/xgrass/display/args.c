#include "xgdisp.h"

void
#ifdef _NO_PROTO
ParseString(r, n, st)
     char ***st, *r;
     int *n;
#else
ParseString(char *r, int *n, char ***st)
#endif     
{
  char *t, *str;
  int loop = 0, len = 0, pos = 0;
  
  for (loop = 0; loop < strlen(r); loop++)
    if (r[loop] == '\n')
      ++len;

  str = (char *) malloc (strlen(r) + 1);
  *n = 0;
  *st = (char **) malloc ((len + 1) * sizeof(char *));

  t = r;
  str[0] = '\0';
  
  while (*t){
    switch(*t)
      {
      case '\n':
	(*st)[*n] = (char *) malloc (strlen(str) + 1);
	strcpy((*st)[*n], str);
	str[0] = '\0';
	++(*n);
	pos = 0;
      default:
	break;
      }
    
    if ((pos == 0) && (*t == '\n'))
      ++t;
    else{
      str[pos++] = *t;
      ++t;
      str[pos] = '\0';
    }
  }
  
  if (str[0] != '\0'){
    (*st)[*n] = (char *) malloc (strlen(str) + 1);
    strcpy((*st)[*n], str);
    str[0] = '\0';
    ++(*n);
  }
}

