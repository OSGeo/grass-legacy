#include <stdio.h>

delcr(str)   /* char *str   */
   char *str;
{

        str[strlen(str)-1] = NULL;
	printf("%s",str);
}
	
