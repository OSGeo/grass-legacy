#include "globals.h"
#include "display.h"
/** #include "Vect.h" **/
#include <signal.h>

int interrupt;
static int set_signals(void);
static int reset_signals(void);
static void sigint(int);

#define GRID_X 10
#define GRID_Y 10

int grid1 (int  zoom)  /* 0 - VIEW_MAP1; 1 - VIEW_MAP1_ZOOM */
{
  int i, j;
  double x[2], y[2];
  double n,s,e,w, ns_dist, ew_dist;
  double d_top, d_bottom, d_left, d_right;

  if (zoom == 0) {  	/* VIEW_MAP1 */
    d_top    = (VIEW_MAP1->vect.top); 
    d_bottom = (VIEW_MAP1->vect.bottom); 
    d_left   = (VIEW_MAP1->vect.left); 
    d_right  = (VIEW_MAP1->vect.right); 
    
    G_set_window (&VIEW_MAP1->vect.head); 
    
    D_set_clip_window((int) d_top, (int) d_bottom, 
		      (int) d_left, (int) d_right);
    
    D_do_conversions(&VIEW_MAP1->vect.head, 
		     (int) d_top, (int) d_bottom, 
		     (int) d_left, (int) d_right);
    
    G_setup_plot (  d_top, d_bottom, d_left,  d_right,
		  D_move_abs, D_cont_abs);
    
  }
  else if (zoom == 1) {  	/* VIEW_MAP1_ZOOM */
    
    d_top    = (VIEW_MAP1_ZOOM->vect.top); 
    d_bottom = (VIEW_MAP1_ZOOM->vect.bottom); 
    d_left   = (VIEW_MAP1_ZOOM->vect.left); 
    d_right  = (VIEW_MAP1_ZOOM->vect.right); 
    
    G_set_window (&VIEW_MAP1_ZOOM->vect.head); 
      
    D_set_clip_window((int) d_top, (int) d_bottom, 
		      (int) d_left, (int) d_right);
    
    D_do_conversions(&VIEW_MAP1_ZOOM->vect.head, 
		     (int) d_top, (int) d_bottom, 
		     (int) d_left, (int) d_right);
    
    G_setup_plot (  d_top, d_bottom, d_left,  d_right,
		  D_move_abs, D_cont_abs);
  }


  /* fprintf (stderr, "Plotting Vectors... "); fflush (stdout); */
  n = VIEW_MAP1->vect.head.north;
  s = VIEW_MAP1->vect.head.south;
  e = VIEW_MAP1->vect.head.east;
  w = VIEW_MAP1->vect.head.west;
  ns_dist = n - s;
  ew_dist = e - w;

  /* plot horizontal lines */
  x[0] = w;
  x[1] = e;
  for (i = 0; i <= GRID_Y; i++) {
    y[0] = y[1] = (s + (i * (ns_dist / GRID_Y)));
    G_plot_line(x[0], y[0], x[1], y[1]);
  }

  /* plot verticle lines */
  y[0] = s;
  y[1] = n;
  for (j = 0; j <= GRID_X; j++) {
    x[0] = x[1] = (w + (j * (ew_dist / GRID_X)));    
    G_plot_line(x[0], y[0], x[1], y[1]);
  }

  return 0;
}

/*************************************************************
 ** grid1_warp  (zoom, E, N, order)
 **    int  zoom;     0 - VIEW_MAP2; 1 - VIEW_MAP2_ZOOM
 **    double E[];
 **    double N[];
 **    int  order;
**************************************************************/

int grid1_warp  (zoom)
int  zoom;     /* 0 - VIEW_MAP2; 1 - VIEW_MAP2_ZOOM */
{
  int i, j;
  double x[2], y[2], z[2];
  double n,s,e,w, ns_dist, ew_dist;
  double d_top, d_bottom, d_left, d_right;
  double x0, y0, z0, x1, y1, z1;

  if (zoom == 0) {  	/* VIEW_MAP2 */
    d_top    = (VIEW_MAP2->vect.top); 
    d_bottom = (VIEW_MAP2->vect.bottom); 
    d_left   = (VIEW_MAP2->vect.left); 
    d_right  = (VIEW_MAP2->vect.right); 
    
    G_set_window (&VIEW_MAP2->vect.head); 
    
    D_set_clip_window((int) d_top, (int) d_bottom, 
		      (int) d_left, (int) d_right);
    
    D_do_conversions(&VIEW_MAP2->vect.head, 
		     (int) d_top, (int) d_bottom, 
		     (int) d_left, (int) d_right);
    
    G_setup_plot (  d_top, d_bottom, d_left,  d_right,
		  D_move_abs, D_cont_abs);
    
  }
  else if (zoom == 1) {  	/* VIEW_MAP2_ZOOM */
    
    d_top    = (VIEW_MAP2_ZOOM->vect.top); 
    d_bottom = (VIEW_MAP2_ZOOM->vect.bottom); 
    d_left   = (VIEW_MAP2_ZOOM->vect.left); 
    d_right  = (VIEW_MAP2_ZOOM->vect.right); 
    
    G_set_window (&VIEW_MAP2_ZOOM->vect.head); 
    
    D_set_clip_window((int) d_top, (int) d_bottom, 
		      (int) d_left, (int) d_right);
    
    D_do_conversions(&VIEW_MAP2_ZOOM->vect.head, 
		     (int) d_top, (int) d_bottom, 
		     (int) d_left, (int) d_right);
    
    G_setup_plot (  d_top, d_bottom, d_left,  d_right,
		  D_move_abs, D_cont_abs);
  }


  /* fprintf (stderr, "Plotting Vectors... "); fflush (stdout); */
  n = VIEW_MAP1->vect.head.north;
  s = VIEW_MAP1->vect.head.south;
  e = VIEW_MAP1->vect.head.east;
  w = VIEW_MAP1->vect.head.west;
  ns_dist = n - s;
  ew_dist = e - w;


  /* try to catch the interupt */
  set_signals() ;
  interrupt = 0 ;
  fprintf(stderr, "\nPlotting warped grid\n") ;
  fprintf(stderr, "      Hit %s to abort plotting\n", G_unctrl(G_intr_char())) ;


  /* plot verticle  lines */
  for (i = 0; i <= GRID_X; i++) {
    x[0] = x[1] = (w + (i * (ew_dist / GRID_X)));    

    for (j = 0; j < GRID_Y; j++) {
      if (interrupt) {
         reset_signals();
         return(0);
      }

      y[0] = (s + (j * (ns_dist / GRID_Y)));
      y[1] = (s + ((j+1) * (ns_dist / GRID_Y)));

      /** TODO -- get elevation**/
      z[0] = z[1] = 0.0;
      group.forward_trans (&group, x[0], y[0], z[0], &x0, &y0, &z0);
      group.forward_trans (&group, x[1], y[1], z[1], &x1, &y1, &z1);

      G_plot_line(x0, y0, x1, y1);
    }
  }


  /* plot horizontal lines */
  for (i = 0; i <= GRID_Y; i++) {
    y[0] = y[1] = (s + (i * (ns_dist / GRID_Y)));

    for (j = 0; j < GRID_X; j++) {
      if (interrupt) {
         reset_signals();
         return(0);
      }

      x[0] = (w + (j * (ew_dist / GRID_X)));    
      x[1] = (w + ((j+1) * (ew_dist / GRID_X)));

      /** TODO -- get elevation**/
      z[0] = z[1] = 0.0;
      group.forward_trans (&group, x[0], y[0], z[0], &x0, &y0, &z0);
      group.forward_trans (&group, x[1], y[1], z[1], &x1, &y1, &z1);

      G_plot_line(x0, y0, x1, y1);
    }
  }
  fprintf(stderr, "\nDone plotting warped grid\n") ;

  return 0;
}



static int set_signals()
{
/* set the ctrlz ignore */
#ifdef SIGTSTP
    signal (SIGTSTP, SIG_IGN);
#endif
 
/* set other signal catches */
 
    interrupt = 0;
 
    signal (SIGINT, sigint);
  return 0;
}
 
static int reset_signals()
{
#ifdef SIGTSTP
    signal (SIGTSTP, SIG_DFL);
#endif
    signal (SIGINT, SIG_DFL);
  return 0;
}
 
static void sigint(int n)
{
    signal (n,sigint);
    interrupt = n;
}
