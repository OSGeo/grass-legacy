			/********************************/
			/*     r.le.setup/mv_wind.c 	*/
			/*				*/
			/*             2.2              */
			/*				*/
			/*       12/1/97 version        */
			/*				*/
			/*      Programmer: Baker       */
			/*      Univ. of Wyoming 	*/
			/********************************/

#include "setup.h"



				/* SETUP THE PARAMETERS FOR THE 
				   MOVING WINDOW */

void  mov_wind(t, b, l, r, n1, n2, n3, mx)
double  *mx;
int     t, b, l, r;
char    *n1, *n2, *n3;
{

  int    xp=0, yp=0, x1, y1, btn=0, s, l0, t0, xpl, ypt,
         u_w=0, u_l=0, u_w0, u_l0, w_w=0, w_l=0, initl=0,
         initt=0, d, fmask, j, circle=0;
  register int i;
  double tmp[2], tmp1, radius=0.0;
  FILE   *fp;

/*
	mx[0]	=	cols of region/width of screen
	mx[1]	=	rows of region/height of screen
	t	=	top row of sampling frame
	b	=	bottom row of sampling frame
	l	=	left col of sampling frame
	r	=	right col of sampling frame
*/

				/* open the moving window parameter file */

  fp = fopen0("r.le.para/move_wind", "w");  
  G_sleep_on_error(0);

  initl = l;
  initt = t;

  l = (int)((double)(l * mx[0]) + 0.5);
  r = (int)((double)(r * mx[0]) + 0.5);
  t = (int)((double)(t * mx[1]) + 0.5);
  b = (int)((double)(b * mx[1]) + 0.5);

				/* display sampling frame */

  R_open_driver();  
  R_standard_color(D_translate_color("grey"));
  draw_box((int)(l/mx[0] + 0.5), (int)(t/mx[1] + 0.5), (int)(r/mx[0] + 0.5),
     (int)(b/mx[1] + 0.5), 1);
  R_close_driver();

				/* determine whether the user will use
				   the keyboard or mouse to setup the 
				   moving window */

keyboard:
  printf("\n\n    HOW WILL YOU SPECIFY THE MOVING WINDOW?\n");
  printf("\n       Use keyboard to enter moving window dimensions   1");
  printf("\n       Use mouse to draw moving window                  2\n");
  printf("\n                                            Which Number?  ");
  numtrap(1, &tmp1);
  d = (int)(tmp1);
  if (d < 1 || d > 2) {
     printf("     You did not enter a 1 or 2, try again\n");
     goto keyboard;
  }

  if (d == 1 || d == 2) {
                                /* return a value > 0 to fmask if there is
				   a MASK present */

     printf("\n    If a MASK is not present (see r.mask) a beep may sound\n");
     printf("    and a WARNING may be printed that can be ignored.\n");
     printf("    If a MASK is present there will be no warning.\n");
     fmask = G_open_cell_old("MASK",G_mapset());
     printf("\n");

				/* setup the moving window using keyboard */
 
     if (d == 1) {
				/* if sampling using circles */

        printf("\n    Do you want to sample using rectangles");
        if (!G_yes("\n       (including squares) (y) or circles (n)?   ",1)) {
           printf("\n    What radius do you want for the circles?  Radius");
           printf("\n       is in pixels; add 0.5 pixels, for the center"); 
           printf("\n       pixel, to the number of pixels outside the"); 
           printf("\n       center pixel.  Type a real number with one"); 
           printf("\n       decimal place ending in .5 (e.g., 4.5):        ");
           numtrap(1, &radius);
           u_w = (int)(2 * radius);
           u_l = (int)(2 * radius);
           u_w0 = u_w/mx[0];
           u_l0 = u_l/mx[1];
        }

				/* if sampling using rectangles/squares */

        else {
back:
           printf("\n    Enter number of COLUMNS & ROWS for the dimensions of");
           printf("\n       the moving window (e.g., 10 10):  ");
           numtrap(2, tmp);   
           u_w = fabs(tmp[0]);
           u_l = fabs(tmp[1]);
           u_w0 = fabs(tmp[0])/mx[0];   
           u_l0 = fabs(tmp[1])/mx[1];

				/* trap possible errors in dimensions */

           if (!u_w0 || !u_l0) { 
              printf("\n    You entered a dimension as 0; enter dimensions again\n");
              goto back;
           }
           else if (u_w == 1 && u_l == 1) { 
              printf("\n    You entered dimensions as 1 1; This will not produce");
              printf("\n       meaningful results; enter larger dimensions\n");
              goto back;
           }
           else if (u_w >= r || u_l >= b) {
              printf("\n    Window size you chose allows < 2 windows across each row;");
              printf("\n       please make window dimensions smaller\n");
              goto back;
           }
        }

				/* display the user-defined moving
				   window on the map */
      
        R_open_driver();
        R_standard_color(D_translate_color("red"));
        if (radius) {
	   draw_circle(initl, initt, initl+u_w0, initt+u_l0, 3);
        }
        else {
           draw_box(initl, initt, initl+u_w0, initt+u_l0, 1);
        }
        R_close_driver();

				/* if all is OK, then set window dimensions */

        printf("\n    Is the displayed moving window as you wanted it (y) or");
        if (G_yes("\n       do you want to redo it? (n)     ", 1)) { 
           xp = (int)(u_w0);
           yp = (int)(u_l0);
        }
        else {
           paint_map(n1, n2, n3);
	   R_open_driver();  
           R_standard_color(D_translate_color("grey"));
           draw_box((int)(l/mx[0] + 0.5), (int)(t/mx[1] + 0.5), (int)(r/mx[0] + 0.5),
              (int)(b/mx[1] + 0.5), 1);
           R_close_driver();
	   radius = 0.0;
	   goto keyboard;
        }
     }

				/* setup the moving window using the mouse */

     else if (d == 2) {
        G_system("clear");

				/* if sampling using circles */

        printf("\n\n    Do you want to use a rectangular (including squares) (y)"); 
        if (!G_yes("\n       or circular (n) moving window?   ",1)) {
           circle = 1;
           printf("\n    Draw a rectangular area to contain a circular moving window.");
           printf("\n    First select upper left corner, then lower right:\n");
           printf("       Left button:     Check unit size\n");
           printf("       Middle button:   Upper left corner of area here\n");
           printf("       Right button:    Lower right corner of area here\n");
        }
        else {
           circle = 0;
           printf("\n    Draw a rectangular (or square) moving window");
           printf("\n    First select upper left corner, then lower right:\n");
           printf("       Left button:     Check moving window size\n");
           printf("       Middle button:   Upper left corner of window here\n");
           printf("       Right: button:   Lower right corner of window here\n");
        }
        R_open_driver();
        while(btn != 3){
back1:
           R_get_location_with_box(l, t, &xp, &yp, &btn);
           u_w = (int)((double)(xp-l)*mx[0] + 0.5); 	
           u_l = (int)((double)(yp-t)*mx[1] + 0.5);

           if(btn == 1) {  	/** show the size and ratio **/
              printf("    Window would be %d columns wide by %d rows long\n",
                          u_w, u_l);
              printf("    Width/length would be %5.2f and area %d pixels\n",
                          (float)u_w/u_l, u_w*u_l);
              for(i = 0; i < 120; i++)
                 fprintf(stdout,"\b");
              fflush(stdout);
           }
  
           else if(btn == 2){
              R_move_abs(xp, yp);
	      l0 = l;
	      t0 = t;
              l = xp;   
              t = yp;
           }
 
           else if( btn == 3) {
              xpl = (int)((double)((int)((double)(xp-l)*mx[0] + 0.5))/mx[0]);
              ypt = (int)((double)((int)((double)(yp-t)*mx[1] + 0.5))/mx[1]);
	      if (xpl < 0 || ypt < 0) {
	         printf("\n    You did not put lower right corner below and to the");
                 printf("\n       of upper left corner. Please select lower right");
                 printf("\n       corner again");
                 goto back1;
              }
              else if (xpl == 0 || ypt == 0) {
	         printf("\n\n    Window would have 0 rows and/or 0 columns;");
                 printf("       try again\n");
	         goto back1;
              }
              else if (xpl > 0 && ypt > 0) {
                 R_standard_color(D_translate_color("red"));
                 if (circle) {
                    if (xpl > ypt) xpl = ypt;
                    else if (ypt > xpl) ypt = xpl;
                    u_w = (int)((double)xpl*mx[0] + 0.5);
                    u_l = (int)((double)ypt*mx[1] + 0.5);
                    draw_circle(initl, initt, initl+xpl, initt+ypt, 3);
                 }
                 else
                    draw_box(initl, initt, initl+xpl, initt+ypt, 1);

	         G_system("clear");
                 if (circle) {
		    radius = (float)u_w/2.0;
	            printf("\n\n    Circular moving window has radius = %5.2f pixels\n",
	               radius);
                 }
                 else {
	            printf("\n    Rectangular moving window has %d columns and %d rows"); 
                    printf("\n    with width/length ratio of %5.2f and area of %d pixels\n", 
                       u_w, u_l, (float)u_w/u_l, u_w*u_l);
                 }
              }
           }		           	
        }
        R_close_driver();
        l = l0;
        t = t0;
     }

     G_close_cell(fmask);
  }

				/* if neither, then exit */

  else 
     exit(0);
				/* write the moving window parameters
				   into the r.le.para/move_wind file */
 
  fprintf(fp, "%8d%8d  u_w u_l: CELL\n", u_w, u_l);

  w_w = r-l;
  w_l = b-t;

				/* write the radius of circles, if a
				   circular moving window is to be used */

  fprintf(fp, "%8.1f          radius of circular moving window\n",radius);

 				/* write the search area in the 
				   r.le.para/move_wind file */

  fprintf(fp, "%8d%8d  w_w w_l\n",w_w,w_l);
  fprintf(fp, "%8d%8d  x0, y0\n", (int)((double)(initl)*mx[0] + 0.5), 
     (int)((double)(initt)*mx[1] + 0.5));  

  fclose(fp);
}

