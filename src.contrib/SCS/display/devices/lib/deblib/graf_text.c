

/*---------- Function: graf_text ----------*/
#include <stdio.h>
graf_text(x1, y1, chrstrng)
int x1, y1;
char *chrstrng;
{
    int leng;

    put_chr('T');
    put_int(x1);
    put_int(y1);
    leng = strlen(chrstrng);
    put_int(leng);
    fwrite(chrstrng, 1, leng, stdout);
    fflush(stdout);
}
