/* %W% %G% */
#include "P.h"
char panel_hd[82];

Ptext (s) 
    char *s;
{

    char *t;
    static char PANEL[]={'p','a','n','e','l'};
    unsigned char n1c,n2c,ch1,ch2;
    unsigned int  m,cnt,n1,n2;
    unsigned int  i,j,k,l;

    /* get letter count in the text line */
    t = s;
    cnt = 0;
    while(*t >= ' ' && *t < 0177)
    {
        cnt++;
        t++;
    }

    /* start converting text into raster data */
    ndots = ndots + CHARHGT;
    if (ndots > NDOTSV)
    {
        Pfinish();
        ndots = CHARHGT + LINSPAC;
    }
    else
    {
        ndots += LINSPAC;
    }

    /* Charter font width = 16 dots = 16 bits = 2 byte */
    cnt = cnt * 2 + LM;
    n1 = cnt / 256;
    n2 = cnt % 256;
    n1c = (unsigned char)(n1);
    n2c = (unsigned char)(n2);
    j = 0;
    for (k=0; k<CHARHGT; k++)
    {
        putc(ESC,yellow_f);
        putc('K',yellow_f);
        putc(n1c,yellow_f);
        putc(n2c,yellow_f);
        for (l=0; l<LM; l++)
            putc((char)0x00,yellow_f);
        putc(ESC,magenta_f);
        putc('K',magenta_f);
        putc(n1c,magenta_f);
        putc(n2c,magenta_f);
        for (l=0; l<LM; l++)
            putc((char)0x00,magenta_f);
        putc(ESC,cyan_f);
        putc('K',cyan_f);
        putc(n1c,cyan_f);
        putc(n2c,cyan_f);
        for (l=0; l<LM; l++)
            putc((char)0x00,cyan_f);
        t = s;
        while (*t >= ' ' && *t < 0177)
        {
            m = *t++;
            i = m - 32;
            ch1 = plotfont[i][j];
            ch2 = plotfont[i][j+1];
            putc(ch1,yellow_f);
            putc(ch1,magenta_f);
            putc(ch1,cyan_f);
            putc(ch2,yellow_f);
            putc(ch2,magenta_f);
            putc(ch2,cyan_f);
        }
        j = j + 2;
    }

    /* spacing between two lines */
    for (k=0; k<LINSPAC; k++)
    {
        putc(ESC,yellow_f);
        putc('K',yellow_f);
        putc((char)0x00,yellow_f);
        putc((char)0x14,yellow_f);
        for (l=0; l<LM; l++)
            putc((char)0x00,yellow_f);
        putc(ESC,magenta_f);
        putc('K',magenta_f);
        putc((char)0x00,magenta_f);
        putc((char)0x14,magenta_f);
        for (l=0; l<LM; l++)
            putc((char)0x00,magenta_f);
        putc(ESC,cyan_f);
        putc('K',cyan_f);
        putc((char)0x00,cyan_f);
        putc((char)0x14,cyan_f);
        for (l=0; l<LM; l++)
            putc((char)0x00,cyan_f);
    }
}
