#include <stdio.h>
#include <math.h>


main()
{
double area, dots, x, y, x1, x2, y1, y2, dx, dy, factor, ratio;
int i,ix ,iy, iix, iiy, r, r1, tx, ty;

scanf("%lf %lf %lf %lf %lf",&x1,&x2,&y1,&y2,&dots);
printf("%lf %lf %lf %lf %lf",x1,x2,y1,y2,dots);

area = (x2 - x1) * (y2 - y1);
ratio = area/dots;
factor = sqrt(ratio);
ix = (x2 - x1)/(factor);
iy = (y2 - y1)/(factor);
r = dots - (ix * iy);
tx = ix;
ty = iy;

if (r > 0) {
	r = ((ix+1) * (iy+1)) - dots;
	for (iix = ix -1; iix <= ix +1; iix++)
		for (iiy = iy -1; iiy <= iy +1; iiy++) {
			r1 = (iix * iiy) - dots ;
			if (r1 > 0 && r1 < r) {
				r = r1;
				tx = iix;
				ty = iiy;
				}
			}
	}

ix = tx;
iy = ty;

fprintf(stderr,"ix%d iy%d\n",ix,iy);

dx = (x2-x1)/(ix+0.9);
dy = (y2-y1)/(iy+0.9);
fprintf(stderr,"A_%lf R_%lf F_%lf\nX_%lf Y_%lf\n",area,ratio,factor,dx,dy);
x = x1 + dx;
i = 0;

while ( x < x2) {
	y = y1 + dy;
	while ( y < y2) {
		printf("%d_  x_%lf y_%lf\n",++i,x,y);
		y += dy;
		}
	x += dx;
	}
}
