/* %W% %G% */
#include <stdio.h>
#include "P.h"
Prle (buf, n)
    char *buf;
    int n;
{

    char *bufdata;
    int i,j;
    unsigned int len,itemp1;
    unsigned char ctemp1,value;
    char *cptr1;
 
    extern char *malloc();

    len = 0;
    cptr1 = buf;
    for (i=0; i<n; i++)
    {
        ctemp1 = *cptr1++;
        itemp1 = ctemp1;
        len += itemp1;
        cptr1++;
    }
    bufdata = malloc(len);
    cptr1 = bufdata;
    for (i=0; i<n; i++)
    {
        ctemp1 = *buf++;
        itemp1 = ctemp1;
        value = *buf++;
        for (j=0; j<itemp1; j++)
        {
            *cptr1 = value;
            cptr1++;
        }
    }
    Pdata(bufdata,len);
    free(bufdata);
}
