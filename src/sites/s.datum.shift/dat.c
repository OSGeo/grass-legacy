#include "gis.h"
#include "CC.h"

char *
datum_list(void)
{
  int len, n;
  char *list;
  char *name;
  
  len=0;
  for (n=0; (name = CC_datum_name(n)) != NULL; n++)
    len += strlen(name)+1;
  list = G_malloc(len);
  for (n=0; (name = CC_datum_name(n)) != NULL; n++)
    {
      if (n) 
	G_strcat (list, ",");
      else 
	*list = '\0';
      G_strcat (list, name);
    }
  return list;
}
