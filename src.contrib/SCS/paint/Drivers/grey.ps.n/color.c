#include <stdio.h>
#define LINE  10
color(n)
{
	static int cnt=0;
    float red,grn,blu;
    int r,g,b;
	char *hexits = "0123456789abcdef";

    Pcolorvalue(n,&red,&grn,&blu);
    r = red * 255;
    g = grn * 255;
    b = blu * 255;
		{
    	Poutc(hexits[r >> 4]);
    	Poutc(hexits[r & 15]);
    	Poutc(hexits[g >> 4]);
    	Poutc(hexits[g & 15]);
    	Poutc(hexits[b >> 4]);
    	Poutc(hexits[b & 15]);
		if (++cnt == LINE){
			cnt = 0;
		Poutc('\n');
			}
		}

}

