

/*
**  code to write data to tmp file 
*/
#include "cell.h"

extern int SCREEN_LEFT   ;
extern int SCREEN_RIGHT  ;
extern int SCREEN_BOTTOM ;
extern int SCREEN_TOP    ;

int store_xy (int x, int y)
{
    if ( x < SCREEN_LEFT || x > SCREEN_RIGHT ||
	 y < SCREEN_TOP  || y > SCREEN_BOTTOM )
	    return 0;

/*DEBUG fprintf (stderr, "STORE_XY  (%d,%d)  %d\n", x, y, Cur_color); */
    fseek (Temp_fp, (long) (y-1)*SCREEN_RIGHT+x-1, 0);
    fwrite (&Cur_color, 1, 1,  Temp_fp);

    return 0;
}

int horiz_line (int y, int x1, int x2)
{
    register int i, len;

    if (( y  < SCREEN_TOP   || y  > SCREEN_BOTTOM ) ||
	( x1 < SCREEN_LEFT  && x2 < SCREEN_LEFT   ) ||
	( x1 > SCREEN_RIGHT && x2 > SCREEN_RIGHT  ))
	    return 0;

    x1 = (x1 < SCREEN_LEFT ?
    		SCREEN_LEFT:
		(x1 > SCREEN_RIGHT ?
		 	SCREEN_RIGHT:
			x1));

    x2 = (x2 < SCREEN_LEFT ?
    		SCREEN_LEFT:
		(x2 > SCREEN_RIGHT ?
		 	SCREEN_RIGHT:
			x2));

    if ( x1 > x2 ){
	i = x1;
	x1 = x2;
	x2 = i;
    }

    len = x2-x1+1;
    for (i = 0 ; i < len ; i++)
	Row_buf[i] = Cur_color;

    fseek (Temp_fp, (long) (y-1)*SCREEN_RIGHT+x1-1, 0);
    fwrite (Row_buf, 1, len,  Temp_fp);
    return (0);
}
