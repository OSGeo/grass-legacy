/* %W% %G% */
#include "P.h"


Pdata (buf, n)
    unsigned char *buf;
{
    int n1,n2,nn;
    char n1c,n2c;
    char cha[4],chb[NCOLBITS];
    int  i,len;
    int  round;
    unsigned char *buft;

    /* the first 8 bytes will be discarded by Plotmaster */
    /* additional 20 bytes are to centralize the picture */
    /* So, LM = 20+8 = 28                                */
    /* each color code will be printed in 4 dots forming */
    /* a rectangle                                       */
    nn = (n*2 + 7) / 8 + LM;
    n1 = nn / 256;
    n2 = nn % 256;
    n1c = (unsigned char)(n1);
    n2c = (unsigned char)(n2);

    for (round=0; round < 2; round++)
    {
        ndots++;
        if (ndots > (NDOTSV-1))
        {
            Pfinish();
            ndots = 0;
        }
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

        len = n*2;
        buft = buf;
        while (len > 0)
        {
            if (len >= 4)
            {
                for (i =0; i < 4; i++)
                {
                    cha[i] = *buft;
                    buft++;
                };
                len -= 4;
            }
            else
            {
                for (i = 0; i < len; i++)
                {
                    cha[i] = *buft;
                    buft++;
                };
                for (i = len; i < 4; i++)
                    cha[i] = 0x00;
                len = 0;
            }

           Ppixel8(round,cha,chb);
      
           putc(chb[0],yellow_f);
           putc(chb[1],magenta_f);
           putc(chb[2],cyan_f);
        }
    }
}

Ppixel8(round,ch4in,ch3out)
    int  round;
    char ch4in[4],ch3out[NCOLBITS];
{
    int  i,j;
    unsigned int  colcode,intensity;
    static char mask[2][5]={0x00,0x02,0x02,0x03,0x03,
                            0x00,0x00,0x01,0x01,0x03};
    
    for (j = 0; j < NCOLBITS; j++)
        ch3out[j] = 0x00;

    for (i = 0; i < 4; i++)
    {
        colcode = (unsigned int) ch4in[i];
        for (j=0; j<NCOLBITS; j++)
        {
            ch3out[j] = ch3out[j] << 2;
            intensity = colcode % 5;
            ch3out[j] = ch3out[j] | mask[round][intensity];
            colcode /= 5;
        }
    }
    for (j=0; j<NCOLBITS; j++)
        ch3out[j] = ch3out[j] ^ (char)0xff;
}
