
#include <stdio.h>
#include "gis.h"
#include "options.h"

char *fgets();

DrawIcons(fp,size,window)
FILE *fp;
int size;
struct Cell_head *window;
{
int i,j,c;
int h,w,cx,cy;
int row,col;
double U_X, U_Y ;
int D_X, D_Y ;
double D_u_to_d_col() ;
double D_u_to_d_row() ;

IconSize(fp,&h,&w,&cx,&cy);
if (h==0 || w==0)
   draw_points_x(size,*window);

while(next_point(window, &U_X, &U_Y))
   {
   D_X = (int)D_u_to_d_col(U_X) ;
   D_Y = (int)D_u_to_d_row(U_Y) ;
   row = D_Y-cy;
   col = D_X-cx;
   j=0;
   for (i=row; i<=(row+h); i++)
      {
      do {
         c=fgetc(fp);
         if (c!='\n')
            dot(c,(j+col),i); 
         j++;
         }
      while (c!='\n' && c!=EOF);
      j=0;
      }
   rewind(fp);
   }
}

dot(flag,x,y)
int flag,x,y;
{
if (flag=='x' || flag=='+')
   {
   R_move_abs(x,y);
   R_cont_abs(x,y);
   }
}

next_point (window, U_X, U_Y)
	struct Cell_head *window;
	double *U_X;
	double *U_Y;
{
	char buffer[128];

	do
	{
	    if(!fgets(buffer, sizeof buffer, infile)) return 0;
	    if (sscanf (buffer, "%lf %lf", U_X, U_Y) != 2) return 0;
	}
	while(*U_X < window->west || *U_X > window->east ||
	      *U_Y < window->south || *U_Y > window->north) ;

	return 1;
}

draw_points_x(size,window)
	struct Cell_head *window;
{
	double U_X, U_Y ;
	int D_X, D_Y ;
	double D_u_to_d_col() ;
	double D_u_to_d_row() ;

	while(next_point(window, &U_X, &U_Y))
	{
		D_X = (int)D_u_to_d_col(U_X) ;
		D_Y = (int)D_u_to_d_row(U_Y) ;
		R_move_abs(D_X-size, D_Y-size) ;
		R_cont_abs(D_X+size, D_Y+size) ;
		R_move_abs(D_X+size, D_Y-size) ;
		R_cont_abs(D_X-size, D_Y+size) ;
	}
}
