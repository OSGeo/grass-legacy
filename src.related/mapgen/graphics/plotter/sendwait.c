#include <stdio.h>
#define SIZE 127

main()
{
int w,i;
char c;

w = 10;
c = ' ';
while (c != EOF) {
for(i=0;i < SIZE;i++){
	scanf("%c",&c);
	if (c == EOF) i = SIZE;
	else printf("%c",c);
	}
fprintf(stderr,"w=%d",w);
sleep(w);
}
}
