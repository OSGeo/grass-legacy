			/********************************/
			/*     r.le.setup/mv_wind.c 	*/
			/*				*/
			/*             2.1              */
			/*				*/
			/*       07/25/94 version       */
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
         initt=0;
  register int i;
  double tmp[2];
  FILE   *fp;

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

				/* query the user about defining the
				   moving window, and if to be defined
				   manually */

  G_system("clear");
  if (!G_yes("\n    Use mouse to define the moving window?  ", 1)) {
back:	/** read in the parameters from the keyboard **/
     printf("\n    Enter COLUMNS & ROWS of the window (e.g., 10 10):  ");
     numtrap(2, tmp);   
     u_w = fabs(tmp[0]);
     u_l = fabs(tmp[1]);
     u_w0 = fabs(tmp[0])/mx[0];   
     u_l0 = fabs(tmp[1])/mx[1];
     if (!u_w0 || !u_l0 || u_w0 > (r/1.2) || u_l0 > (b/1.2)){
        printf("\n    Illegal moving window size, try again.\n\n");
        goto back;
     }
     xp = (int)(u_w0);
     yp = (int)(u_l0);

				/* display the user-defined moving
				   window on the map */
      
     R_open_driver();
     R_standard_color(D_translate_color("red"));
     draw_box(initl, initt, initl+u_w0, initt+u_l0, 1);
     R_close_driver();
  } 

				/* otherwise, define the moving window
				   using the mouse */

  else {   	
     G_system("clear");  
     puts("\n\n");
     printf("     Left button:     Check window size\n");
     printf("     Middle button:   Move cursor\n");
     printf("     Right: button:   Define the moving window\n\n");
     R_open_driver();
     while(btn != 3){
        R_get_location_with_box(l, t, &xp, &yp, &btn);
        u_w = (int)((double)(xp-l)*mx[0] + 0.5); 	
        u_l = (int)((double)(yp-t)*mx[1] + 0.5);

        if(btn == 1) {  /** show the size and ratio **/
           printf("    Width=%d length=%d w/l=%5.2f size=%6d cells  ", u_w, 
	      u_l,(float)u_w/u_l, u_w*u_l);
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
           R_standard_color(D_translate_color("red"));
           draw_box(initl, initt, initl+xpl, initt+ypt, 1);
        }		           	
     }
     R_close_driver();
     l = l0;
     t = t0;
  }

  if(u_w < 1) return;
 
				/* write the moving window parameters
				   into the r.le.para/move_wind file */
 
  fprintf(fp, "%8d%8d  u_w u_l: CELL\n", u_w, u_l);

  w_w = r-l;
  w_l = b-t;

/*printf("l=%d (int)(l*mx[0])=%d r=%d r-l=%d (int)(w_w*mx[0])=%d mx[0]=%f\n",l,(int)(l*mx[0]),r,r-l,(int)(w_w*mx[0]),mx[0]);
*/
 				/* save the search area in the 
				   r.le.para/move_wind file */

  fprintf(fp, "%8d%8d  w_w w_l\n",w_w,w_l);
  fprintf(fp, "%8d%8d  x0, y0\n", (int)((double)(initl)*mx[0] + 0.5), 
     (int)((double)(initt)*mx[1] + 0.5));  

  fclose(fp);
}

