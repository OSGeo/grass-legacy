/* %W% %G% */
#include <stdio.h>
#include "P.h"

Pfinish()
{
    int c;
    int i;
    int old_alphamd;
    unsigned char buffer[1024];
    extern void rewind();
    extern int fread();
    extern int fclose();

    Pflush();
 
    rewind(yellow_f);
    rewind(magenta_f);
    rewind(cyan_f);

    while ((c=fread(buffer,sizeof(char),1024,yellow_f)) != 0)
        for (i = 0; i < c; i++)
            Poutc(buffer[i]);
    Poutc(FF);
    Pflush();
    Pdelay(DELAYTIME);

    while ((c=fread(buffer,sizeof(char),1024,magenta_f)) != 0)
        for (i = 0; i < c; i++)
            Poutc(buffer[i]);
    Poutc(FF);
    Pflush();
    Pdelay(DELAYTIME);

    while ((c=fread(buffer,sizeof(char),1024,cyan_f)) != 0)
        for (i = 0; i < c; i++)
            Poutc(buffer[i]);
    Poutc((char)0x04);
    Pflush();
    Pdelay(DELAYTIME);    

    if (ndots > NDOTSV)
    {
        old_alphamd = alphamd;
        fclose(yellow_f);
        fclose(magenta_f);
        fclose(cyan_f);
        isinit = 0;
        Pinit();
        alphamd = old_alphamd;
    }
}


/* delay function */
Pdelay(time)
    unsigned int time;   /* time in ? seconds */
{
    int i,j,k;

    /* delay loop */
    for (i=0; i<30000; i++)
        for (j=0; j<time; j++)
            k = 5%2;
}
