/*
 * Start up graphics processing.  Anything that needs to be assigned, set up,
 * started-up, or otherwise initialized happens here.  This is called only at
 * the startup of the graphics driver.
 *
 * The external variables define the pixle limits of the graphics surface.  The
 * coordinate system used by the applications programs has the (0,0) origin
 * in the upper left-hand corner.  Hence,
 *    SCREEN_LEFT < SCREEN_RIGHT
 *    SCREEN_TOP  < SCREEN_BOTTOM 
 *
 * NCOLORS is set to the total number of colors available on the device.  This
 * most certainly needs to be more than 100 (or so).  If you are writing a
 * driver with fewer colors you probably need to provide your own Color(),
 * Color_table_float(), Color_table_fixed(), and
 * Reset_color() routines (see ../lib/{Color.c,Reset_clr.c,Clr_table.c}).
 */

#include <string.h>
#include <stdlib.h>
#include <gd.h>
#include "gis.h"
#include "driverlib.h"


#define MAIN
#include "png.h"

int SCREEN_LEFT	  = 1;
int SCREEN_TOP    = 1;

int SCREEN_RIGHT;
int SCREEN_BOTTOM;

int NCOLORS = gdMaxColors;
int colorTable[gdMaxColors];

int Graph_Set (void) 
{
    char *p;
    int ret;
    int red = 0;
    int green = 0;
    int blue = 0;
    char colorBuffer[3];
    G_gisinit("PNG driver") ;

    if (NULL == (p = getenv ("LOCATION"))) {
	fprintf (stderr, "You must start this driver from within GRASS\n");
        exit (1);
    }

    /*
     * set the 'screen' resolution of the driver
     */

    if (NULL != (p = getenv ("GRASS_WIDTH"))) {
	SCREEN_RIGHT = atoi (p);
        if (SCREEN_RIGHT <= 1) {
	    SCREEN_RIGHT = DEF_WIDTH;
        }
    } else {
	SCREEN_RIGHT = DEF_WIDTH;
    }

    if (NULL != (p = getenv ("GRASS_HEIGHT"))) {
	SCREEN_BOTTOM = atoi (p);
        if (SCREEN_BOTTOM <= 0) {
            SCREEN_BOTTOM = DEF_HEIGHT;
        }
    } else {
	SCREEN_BOTTOM = DEF_HEIGHT;
    }


    /*
     * open the output file
     */

    if (NULL != (p = getenv ("GRASS_PNGFILE"))) {
        if (strlen(p) == 0) {
            p = FILE_NAME;
        }
    } else {
        p = FILE_NAME;
    }
    file_name = p;

    output = fopen(file_name, "w");
    if (output == NULL) {
      fprintf(stderr,"PNG: couldn't open output file %s\n",file_name);
      exit(1);
    }

    /*
     * Creating the image
     */
    
    if (NULL == (p = getenv ("GRASS_BACKGROUNDCOLOR"))) {
        p = "FFFFFF";
    }
    /*
    if (strlen(p)<>6) {
      p = "FFFFFF";
    } 
    */
    im = gdImageCreate(SCREEN_RIGHT, SCREEN_BOTTOM);
    colorBuffer[0] = p[0];
    colorBuffer[1] = p[1];
    colorBuffer[2] = '\0';
    red = (int) strtol(colorBuffer, (char **)NULL, 16);
    colorBuffer[0] = p[2];
    colorBuffer[1] = p[3];
    colorBuffer[2] = '\0';
    green = (int) strtol(colorBuffer, (char **)NULL, 16);
    colorBuffer[0] = p[4];
    colorBuffer[1] = p[5];
    colorBuffer[2] = '\0';
    blue = (int) strtol(colorBuffer, (char **)NULL, 16);
    
    ret = gdImageColorAllocate(im, red, green, blue);
    if(ret == -1) {
      fprintf(stderr, "PNG: color allocation error");
      exit(1);
    }
    currentColor = ret;

    if (NULL == (p = getenv ("GRASS_TRANSPARENT"))) {
      p = "DEFAULT";
    }

    if (strcmp(p,"TRUE") == 0) {
      gdImageColorTransparent(im, currentColor);
    }

    /*
     * Init finished
     */
    
    printf("PNG: collecting to file: %s, width = %d, height = %d\n",
		file_name, SCREEN_RIGHT, SCREEN_BOTTOM);

    fflush(stdout);
    return 0;
}






