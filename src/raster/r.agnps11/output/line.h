/*
 * bar.h
 *
 */

#include <string.h>

float rem();  /* remainder function */ 

/* normalized coordinates of bar-chart components */
 
/* origin */
#define ORIGIN_X        0.10
#define ORIGIN_Y        0.20     

/* y-coordinate of end of y-axis */
#define YAXIS_END       0.80

/* x-coordinate of end of x-axis */
#define XAXIS_END       0.77

/* minimum distance between numbered tic-marks on x-axis */
#define XTIC_DIST	20	

/* minimum distance between numbered tic-marks on y-axis */
#define YTIC_DIST	20	

/* sizes of tic-marks */ 
#define BIG_TIC		0.025
#define SMALL_TIC	0.015

/* y-coordinates of the two text labels */
#define LABEL_1         0.07
#define LABEL_2         0.01
 
/* y-coordinate of x-axis tic-mark numbers */
#define XNUMS_Y		0.14  

/* x-coordinate of y-axis tic-mark numbers */
#define YNUMS_X         0.05     

/* text width and height */
#define TEXT_HEIGHT	0.045
#define TEXT_WIDTH	TEXT_HEIGHT*0.5	
