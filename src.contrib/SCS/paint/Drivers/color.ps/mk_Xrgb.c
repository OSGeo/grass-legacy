#include <stdio.h>
#define LINE 10
#define ROWS 1700
#define COLS 1100
main(argc,argv)
int argc;
char *argv[];
{
FILE *rp, *gp, *bp, *fopen();
char *hexits = "0123456789abcdef";
int cnt;
int r,g,b;



rp = fopen("cell/sub.1","r");
gp = fopen("cell/sub.2","r");
bp = fopen("cell/sub.3","r");

while ((r = getc(rp)) != EOF) {
    g = getc(gp);
    b = getc(bp);
		
    	putchar(hexits[r >> 4]);
    	putchar(hexits[r & 15]);
    	putchar(hexits[g >> 4]);
    	putchar(hexits[g & 15]);
    	putchar(hexits[b >> 4]);
    	putchar(hexits[b & 15]);
		if (++cnt == LINE){
			cnt = 0;
		putchar('\n');
			}
		}

}

