#include <stdio.h>
#include <string.h>
#include <sys/types.h>

main()

{
char s1[50], s2[50], s3[50];

while (scanf("%s %s %s",s1,s2,s3) != EOF) {
	printf("%s\t%s\t%c\n",s1,s2,s3[strlen(s3)-1]);
	}
}
