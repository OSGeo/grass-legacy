#include <stdio.h>
#include <string.h>

#include "ingresUtils.h"

/**************************************************************/
/* FUNCTION: ingresUnTable
 * ARGS:	char buf[1024]
 * 
 *    This function just strips out Ingres table column delimeters
 * and row separators.  Any line that does not begin with a |
 * is cleared, and those that do have all | characters removed.
 */
/**************************************************************/
void ingresUnTable(buf)
char *buf;
{
  char *p;

  p = buf;
  /* skip leading whitespace */
  while(*p && index(" \t\n", *p)) 
    p++;
 
  /* operate on lines beginning with '|', or erase them. */ 
  if( *p == '|' )
  {
    char c,tmpbuf[BUFSIZ];
    int i;

    strncpy(tmpbuf,p,BUFSIZ);
    p = buf;
    for(i=0; c=tmpbuf[i]; i++)
      if( c != '|') 
        *p++ =  c;
      else
	*p++ = ' ';
    *p = 0;
  }
  else
  {
    *buf = 0;
  }
}
/**************************************************************/
