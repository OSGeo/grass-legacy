#include <stdio.h>

main()
{
char c;
int i,max=0,tst=0;

while (tst != 1) 
for (i=0;tst == 0;i++)
	{
	c=getchar();
	if (c == '\n') {if (i > max) max=i;i=0;}
	if (c == EOF) tst=1;
	}
printf("%d ",max);
}
