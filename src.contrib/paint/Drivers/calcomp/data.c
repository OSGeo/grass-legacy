/* %W% %G% */
#include "P.h"
Pdata (buf, n)
    unsigned char *buf;
{
    int n1,n2,nn;
    char n1c,n2c;
    char cha[8],chb[8];
    int  i,len;
    unsigned char *buft;

    ndots++;
    if (ndots > NDOTSV)
    {
        Pfinish();
        ndots = 1;
    }
    /* the first 8 bytes will be discarded by Plotmaster */
    /* additional 20 bytes are to centerize the picture  */
    nn = (n+7) / 8 + LM;
    n1 = nn / 256;
    n2 = nn % 256;
    n1c = (unsigned char)(n1);
    n2c = (unsigned char)(n2);

/* for yellow color plate */
    putc(ESC,yellow_f);
    putc('K',yellow_f);
    putc(n1c,yellow_f);
    putc(n2c,yellow_f);
/* for magenta color plate */
    putc(ESC,magenta_f);
    putc('K',magenta_f);
    putc(n1c,magenta_f);
    putc(n2c,magenta_f);
/* for cyan color plate */
    putc(ESC,cyan_f);
    putc('K',cyan_f);
    putc(n1c,cyan_f);
    putc(n2c,cyan_f);

    /* the first 8 bytes will be discarded by Plotmaster */
    /* additional 20 bytes are to centerize the picture  */
    for (i=0; i<LM; i++)
    {
        putc(NULL,yellow_f);
        putc(NULL,magenta_f);
        putc(NULL,cyan_f);
    }

    len = n;
    buft = buf;
    while (len > 0)
    {
        if (len >= 8)
        {
            for (i =0; i < 8; i++)
            {
                cha[i] = *buft;
                buft++;
            };
            len -= 8;
        }
        else
        {
            for (i = 0; i < len; i++)
            {
                cha[i] = *buft;
                buft++;
            };
            for (i = len; i < 8; i++)
                cha[i] = 0x00;
            len = 0;
        }

       Ppixel8(cha,chb);
  
       putc(chb[0],yellow_f);
       putc(chb[1],magenta_f);
       putc(chb[2],cyan_f);
    }
}

Ppixel8(ch8in,ch8out)
    char ch8in[8],ch8out[8];
{
    int  i,j;
    char cht;
    char maskb[8];
    
    maskb[0] = 0x01;            /* Bit 0 (LSB) */
    maskb[1] = 0x02;            /* Bit 1       */
    maskb[2] = 0x04;            /* Bit 2       */
    maskb[3] = 0x08;            /* Bit 3       */
    maskb[4] = 0x10;            /* Bit 4       */
    maskb[5] = 0x20;            /* Bit 5       */
    maskb[6] = 0x40;            /* Bit 6       */
    maskb[7] = 0x80;            /* Bit 7 (MSB) */

    for (j = 0; j < NCOLBITS; j++)
        ch8out[j] = 0x00;

    for (i = 0; i < 8; i++)
    {
        for (j = 0; j < NCOLBITS; j++)
        {
            ch8out[j] = ch8out[j] << 1;
            cht = ch8in[i] & maskb[j];
            if (cht != (char)0x00) ch8out[j] = ch8out[j] | (char)0x01;
        }
    }
    for (j = 0; j < NCOLBITS; j++)
        ch8out[j] = ch8out[j] ^ (char)0xff;
}
