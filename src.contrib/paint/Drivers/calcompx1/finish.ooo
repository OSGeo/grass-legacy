/* %W% %G% */
#include <stdio.h>
#include "P.h"


Pfinish()
{
    int c;
    int i;
    unsigned char buffer[1024];
    extern void rewind();
    extern int fread();
    extern int fclose();
    extern FILE *tmpfile();

    Pflush();
 
    rewind(yellow_f);
    rewind(magenta_f);
    rewind(cyan_f);

    while ((c=fread(buffer,sizeof(char),1024,yellow_f)) != 0)
        for (i = 0; i < c; i++)
            Poutc(buffer[i]);
    Poutc(FF);
    Pflush();
/*  sleep(30+(unsigned)(ndots/100));
*/
    Pdelay(DELAYTIME);

    while ((c=fread(buffer,sizeof(char),1024,magenta_f)) != 0)
        for (i = 0; i < c; i++)
            Poutc(buffer[i]);
    Poutc(FF);
    Pflush();
/*  sleep(30+(unsigned)(ndots/100));
*/
    Pdelay(DELAYTIME);

    while ((c=fread(buffer,sizeof(char),1024,cyan_f)) != 0)
        for (i = 0; i < c; i++)
            Poutc(buffer[i]);
    Poutc((char)0x04);
    Pflush();
/*  sleep(40+(unsigned)(ndots/100));
*/
    Pdelay(DELAYTIME);    

    fclose(yellow_f);
    fclose(magenta_f);
    fclose(cyan_f);
    if (final_page)
    {
        Pclose();
    }
    else
    {
        yellow_f = tmpfile();
        if ( yellow_f == NULL)
        {
            return(ERROR);
        }

        magenta_f = tmpfile();
        if ( magenta_f == NULL)
        {
            return(ERROR);
        }

        cyan_f = tmpfile();
        if ( cyan_f == NULL)
        {
            return(ERROR);
        }
        rastermd = 0;
        Praster();
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
