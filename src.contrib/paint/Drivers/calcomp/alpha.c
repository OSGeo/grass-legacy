#include <stdio.h>
#include "P.h"

unsigned char plotfont[95][40];
int alphamd;
Palpha()
{
    FILE *font_f;
    char buf[82];
    int x;
    int i,j;

    if (!(alphamd))
    {
        /* font for 20(hex) to 3f(hex) */
        font_f=fopen(FONTF1,"r");
        for (i=0; i<32; i++)
        {
            fgets(buf,82,font_f);
            for (j=0; j<40; j++)
            {
                fscanf(font_f,"%x",&x);
                plotfont[i][j] = (unsigned char)(x);
            }
            fgets(buf,82,font_f);
        }
        fclose(font_f);

        /* font for 40(hex) to 5f(hex) */
        font_f=fopen(FONTF2,"r");
        for (i=32; i<64; i++)
        {
            fgets(buf,82,font_f);
            for (j=0; j<40; j++)
            {
                fscanf(font_f,"%x",&x);
                plotfont[i][j] = (unsigned char)(x);
            }
            fgets(buf,82,font_f);
        }
        fclose(font_f);

        /* font for 60(hex) to 7e(hex) */
        font_f=fopen(FONTF3,"r");
        for (i=64; i<95; i++)
        {
            fgets(buf,82,font_f);
            for (j=0; j<40; j++)
            {
                fscanf(font_f,"%x",&x);
                plotfont[i][j] =(unsigned char)(x);
            }
            fgets(buf,82,font_f);
        }
        fclose(font_f);

        alphamd = 1;
    }
    else
       return;
}
