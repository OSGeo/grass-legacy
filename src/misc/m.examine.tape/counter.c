#include <stdio.h>
counter(n)
{
    char buf[20];
    static int backsp = 0;
    static print_row_msg = 1;
    int len;
    int extra;

    if (n < 0) 
    {
	backsp = 0;
	if (n == -1) fprintf(stderr,"\n");
	print_row_msg = (n == -1);
	return;
    }
    if (print_row_msg)
    {
	fprintf(stderr,"\nnumber of rows: ");
	print_row_msg = 0;
    }

    sprintf(buf,"%d",n);
    len = strlen (buf);
    extra = backsp - len;

    while(backsp--)
	fprintf(stderr,"\b");
    backsp = len;

    fprintf(stderr,"%s",buf);

    for (len = extra; len > 0; len--)
	fprintf(stderr," ");
    for (len = extra; len > 0; len--)
	fprintf(stderr,"\b");
}
