
#include "gis.h"
#include "globals.h"

tig_open(typ)        /*  open a tiger file */
char typ;
{
int i;
  for (i=1; i<=N_TIGERS; i++) {
    if (tiger[i].type == typ) {
      if (! make_open_file(i)) {
        sprintf(t1buf,"Can't open TIGER File Type %c\n%s",
                        tiger[i].type, tiger[i].name);
        G_fatal_error(t1buf);
      }
      break;
    }
  }
}

make_open_file(n)
int n;
{
int len;
char s[250];
  if (tiger[n].fp != NULL) { /* already open */
    rewind(tiger[n].fp);      /* set to start of file */
    return 1;
  }
  if (tiger[n].name == NULL) /* no file name supplied */
      return 0;
  strcpy(s,tiger[n].name);
  if ((len=strlen(s)) == 0) return 0;
  len--;
  if ((tiger[n].fp=fopen(s,"r"))!=NULL) return 1;
  if (s[len] >= 'A' && s[len] <= 'Z') {
      s[len] = s[len] + ('a' - 'A');  /* try lower case */
      strcpy(tiger[n].name,s);
      if ((tiger[n].fp=fopen(s,"r"))!=NULL) return 1;
  }
  return 0;
}

tig_names()
{
int i,len;
char *s;
  for (i=1; i<=N_TIGERS; i++) /* get all specified names */
    if ((s=tiger[i].file) != NULL) {
      tiger[i].name = G_malloc(strlen(s)+3);
      strcpy(tiger[i].name, s);
    }
    else
      tiger[i].name = NULL;

  for (i=1; i<=N_TIGERS; i++) /* fill in others from preceeding names */
    if (tiger[i].name == NULL)
      if ((s=tiger[i-1].name) != NULL) {
        tiger[i].name = G_malloc(strlen(s)+3);
        strcpy(s=tiger[i].name, tiger[i-1].name);
        len = strlen(s);
        s[len-1] = tiger[i].type; s[len] = '\0'; /* change file type */
      }
}

tig_close(typ)
char typ;
{
int i;

  for (i=1; i<=N_TIGERS; i++)
    if(tiger[i].type == typ) {
      if (tiger[i].fp != NULL) {
        fclose(tiger[i].fp);
        tiger[i].fp = NULL;
      }
      break;
   }
}

tig_rewind(typ)
char typ;
{
int i;

  for (i=1; i<=N_TIGERS; i++)
    if(tiger[i].type == typ) {
      if (tiger[i].fp != NULL)
        rewind(tiger[i].fp);
      break;
    }
}
