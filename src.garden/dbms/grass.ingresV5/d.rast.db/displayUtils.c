#include <stdio.h>
#include <string.h>

#include "displayUtils.h"

/**************************************************************/
/* FUNCTION: fancyDisplay.c
 * ARGS:	char name[1024]
 * 
 * This functions display a raster image with title and legend
 *
 * Katarina Johnsson, 930411
 */
/**************************************************************/
void fancyDisplay(name)
char *name;
{
   char sysbuf[1024];

	sprintf(sysbuf,"d.frame -e; d.frame frame=map at=0,79,0,79;\
			d.frame frame=legend at=0,79,80,100;\
			d.frame frame=title at=80,100,0,100;\
			d.frame -s frame=map; d.rast %s;\
			d.frame -s frame=legend; d.legend map=%s lines=30;\
			d.frame -s frame=title; d.title map=%s | d.text",
			name, name, name);
	system(sysbuf);

}
/**************************************************************/
