#include <string.h>
/* copy a structure from a to b */
int 
dig_struct_copy (void *from, void *to, int cnt)
{
  memcpy (to, from, cnt);

  return 0;
}

int 
dig_rmcr (char *str)
{
  int i;

  i = strlen (str) - 1;
  if (str[i] == '\n')
    str[i] = '\0';

  return 0;
}
