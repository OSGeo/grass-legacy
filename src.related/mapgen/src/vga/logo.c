/* Function: logo	Author: Paul W. Carlson		Dec. 1988 
 * 
 * This function displays the SCS logo.
 */ 
#include "vio_driver.h"
#include "logo.h"

logo() 
{ 
    register int *ptr;
    register unsigned char *cptr;
    int x1, x2, y, num;

    color(5);
    Move_abs(517,232); 
    Cont_abs(426, 75); 
    Cont_abs(374, 75); 
    Cont_abs(283, 232); 
    carc(400, 300, 135, 232); 
    Move_abs(478, 255); 
    Cont_abs(400, 120); 
    Cont_abs(322, 255); 
    carc(400, 300, 90, 255); 
    cptr = raster_buff;
    for (num = 0; num < 400; num++) *cptr++ = cur_color;
    ptr = bline;
    for (num = 0; num < 631; num++)
    {	args.arg1 = *ptr++;
    	args.arg2 = *ptr++;
    	args.arg3 = *ptr++;
    	args.ptr  = raster_buff;
    	ioctl(viofd, VIO_HORLINE, &args);
    }
    color(30);
    Move_abs(197, 300);
    Cont_abs(242, 300);
    Move_abs(558, 300);
    Cont_abs(603, 300);
    carc(400, 300, 203, 300); 
    carc(400, 300, 158, 300); 
    cptr = raster_buff;
    for (num = 0; num < 400; num++) *cptr++ = cur_color;
    ptr = gline;
    for (num = 0; num < 359; num++)
    {	args.arg1 = *ptr++;
    	args.arg2 = *ptr++;
    	args.arg3 = *ptr++;
    	args.ptr  = raster_buff;
    	ioctl(viofd, VIO_HORLINE, &args);
    }
    color(215);
    ptr = tline;
    for (num = 0; num < 931; num++)
    {   x1 = *ptr++;
   	y  = *ptr++;
	if (x1 < 0) Move_abs(-x1, -y);
     	else Cont_abs(x1, y);
    }
    Move_abs(198, 8);
    Cont_abs(602, 8);
    quad1(602, 76, 68); 
    Move_abs(670, 76);
    Cont_abs(670, 525);
    quad2(602, 525, 68); 
    Move_abs(198, 593);
    Cont_abs(602, 593);
    quad3(198, 525, 68); 
    Move_abs(130, 76);
    Cont_abs(130, 525);
    quad4(198, 76, 68); 
} 
 


carc(cx, cy, r, min) 
int cx, cy, r, min; 
{ 
    int x, y; 
    long int dx1024, dy1024, prevdx, prevdy, savdx, savdy; 
    long int radius, dx, dy; 
     
    radius = (long)r; 
    dx = prevdx = radius; 
    dx1024 = (dx << 10); 
    for ( dy = 0; dy < dx; dy++) 
    {   dx1024 -= (dy << 10) / dx; 
        dx = ((dx1024 + 512) >> 10); 
	savdx = dx; 
	y = (int)dy; 
	do 
	{   x = (int)dx; 
	    if (cy - y > min) 
	    {	args.arg1 = cx + x;
    		args.arg2 = cy - y;
    		ioctl(viofd, VIO_SETPIX, &args);
    		args.arg1 = cx - x;
    		ioctl(viofd, VIO_SETPIX, &args);
	    } 
	    args.arg1 = cx - x;
    	    args.arg2 = cy + y;
    	    ioctl(viofd, VIO_SETPIX, &args);
	    args.arg1 = cx + x;
    	    ioctl(viofd, VIO_SETPIX, &args);
	} while (++dx < prevdx); 
	dx = savdx; 
	prevdx = dx; 
    } 
    prevdy = dy; 
    dy1024 = (dy << 10); 
    dx--; 
    do 
    {	dy = ((dy1024 + 512) >> 10); 
	savdy = dy; 
	x = (int)dx; 
	do 
	{   y = (int)dy; 
	    if (cy - y > min) 
	    {	args.arg1 = cx + x;
    		args.arg2 = cy - y;
    		ioctl(viofd, VIO_SETPIX, &args);
    		args.arg1 = cx - x;
    		ioctl(viofd, VIO_SETPIX, &args);
	    } 
	    args.arg1 = cx - x;
    	    args.arg2 = cy + y;
    	    ioctl(viofd, VIO_SETPIX, &args);
	    args.arg1 = cx + x;
    	    ioctl(viofd, VIO_SETPIX, &args);
	} while (--dy > prevdy); 
	dy = savdy; 
	prevdy = dy; 
	dy1024 += ((dx - 1) << 10) / (long)y; 
    } while (dx-- >= 0);		 
} 
 
 
quad1(cx, cy, r) 
int cx, cy, r; 
{ 
    int x, y; 
    long int dx1024, dy1024, prevdx, prevdy, savdx, savdy; 
    long int radius, dx, dy; 
     
    radius = (long)r; 
    dx = prevdx = radius; 
    dx1024 = (dx << 10); 
    for ( dy = 0; dy < dx; dy++) 
    {   dx1024 -= (dy << 10) / dx; 
        dx = ((dx1024 + 512) >> 10); 
	savdx = dx; 
	y = (int)dy; 
	do 
	{   x = (int)dx; 
	    args.arg1 = cx + x;
            args.arg2 = cy - y;
            ioctl(viofd, VIO_SETPIX, &args);
	} while (++dx < prevdx); 
	dx = savdx; 
	prevdx = dx; 
    } 
    prevdy = dy; 
    dy1024 = (dy << 10); 
    dx--; 
    do 
    {	dy = ((dy1024 + 512) >> 10); 
	savdy = dy; 
	x = (int)dx; 
	do 
	{   y = (int)dy; 
	    args.arg1 = cx + x;
            args.arg2 = cy - y;
            ioctl(viofd, VIO_SETPIX, &args);
	} while (--dy > prevdy); 
	dy = savdy; 
	prevdy = dy; 
	dy1024 += ((dx - 1) << 10) / (long)y; 
    } while (dx-- >= 0);		 
} 
 
 
quad2(cx, cy, r) 
int cx, cy, r; 
{ 
    int x, y; 
    long int dx1024, dy1024, prevdx, prevdy, savdx, savdy; 
    long int radius, dx, dy; 
     
    radius = (long)r; 
    dx = prevdx = radius; 
    dx1024 = (dx << 10); 
    for ( dy = 0; dy < dx; dy++) 
    {   dx1024 -= (dy << 10) / dx; 
        dx = ((dx1024 + 512) >> 10); 
	savdx = dx; 
	y = (int)dy; 
	do 
	{   x = (int)dx; 
	    args.arg1 = cx + x;
            args.arg2 = cy + y;
            ioctl(viofd, VIO_SETPIX, &args);
	} while (++dx < prevdx); 
	dx = savdx; 
	prevdx = dx; 
    } 
    prevdy = dy; 
    dy1024 = (dy << 10); 
    dx--; 
    do 
    {	dy = ((dy1024 + 512) >> 10); 
	savdy = dy; 
	x = (int)dx; 
	do 
	{   y = (int)dy; 
	    args.arg1 = cx + x;
            args.arg2 = cy + y;
            ioctl(viofd, VIO_SETPIX, &args);
	} while (--dy > prevdy); 
	dy = savdy; 
	prevdy = dy; 
	dy1024 += ((dx - 1) << 10) / (long)y; 
    } while (dx-- >= 0);		 
} 
 
 
quad3(cx, cy, r) 
int cx, cy, r; 
{ 
    int x, y; 
    long int dx1024, dy1024, prevdx, prevdy, savdx, savdy; 
    long int radius, dx, dy; 
     
    radius = (long)r; 
    dx = prevdx = radius; 
    dx1024 = (dx << 10); 
    for ( dy = 0; dy < dx; dy++) 
    {   dx1024 -= (dy << 10) / dx; 
        dx = ((dx1024 + 512) >> 10); 
	savdx = dx; 
	y = (int)dy; 
	do 
	{   x = (int)dx; 
	    args.arg1 = cx - x;
            args.arg2 = cy + y;
            ioctl(viofd, VIO_SETPIX, &args);
	} while (++dx < prevdx); 
	dx = savdx; 
	prevdx = dx; 
    } 
    prevdy = dy; 
    dy1024 = (dy << 10); 
    dx--; 
    do 
    {	dy = ((dy1024 + 512) >> 10); 
	savdy = dy; 
	x = (int)dx; 
	do 
	{   y = (int)dy; 
	    args.arg1 = cx - x;
            args.arg2 = cy + y;
            ioctl(viofd, VIO_SETPIX, &args);
	} while (--dy > prevdy); 
	dy = savdy; 
	prevdy = dy; 
	dy1024 += ((dx - 1) << 10) / (long)y; 
    } while (dx-- >= 0);		 
} 
 
 
quad4(cx, cy, r) 
int cx, cy, r; 
{ 
    int x, y; 
    long int dx1024, dy1024, prevdx, prevdy, savdx, savdy; 
    long int radius, dx, dy; 
     
    radius = (long)r; 
    dx = prevdx = radius; 
    dx1024 = (dx << 10); 
    for ( dy = 0; dy < dx; dy++) 
    {   dx1024 -= (dy << 10) / dx; 
        dx = ((dx1024 + 512) >> 10); 
	savdx = dx; 
	y = (int)dy; 
	do 
	{   x = (int)dx; 
	    args.arg1 = cx - x;
            args.arg2 = cy - y;
            ioctl(viofd, VIO_SETPIX, &args);
	} while (++dx < prevdx); 
	dx = savdx; 
	prevdx = dx; 
    } 
    prevdy = dy; 
    dy1024 = (dy << 10); 
    dx--; 
    do 
    {	dy = ((dy1024 + 512) >> 10); 
	savdy = dy; 
	x = (int)dx; 
	do 
	{   y = (int)dy; 
	    args.arg1 = cx - x;
            args.arg2 = cy - y;
            ioctl(viofd, VIO_SETPIX, &args);
	} while (--dy > prevdy); 
	dy = savdy; 
	prevdy = dy; 
	dy1024 += ((dx - 1) << 10) / (long)y; 
    } while (dx-- >= 0);		 
} 
