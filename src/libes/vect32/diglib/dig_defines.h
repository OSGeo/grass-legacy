/*	 		VERSION    4.0   		*/
#define VERSION "4.10"
#define VERSION_MAJOR	4
#define VERSION_MINOR	10

/* 
**  the earliest version that can read this current format
*/
#define EARLIEST_MAJOR	4
#define EARLIEST_MINOR	0

#define DIGITIZER	0
#define MOUSE		1

#define ON	1
#define OFF	0

/* note this is changed from 
**  3.0  which had 0.04   -dpg
*/
#define THRESH_FUDGE   0.03

#define MAXCOLORS	13
#define MAX_OPEN_FILES 20

#define WHITE	1
#define BLACK	2
#define YELLOW	3
#define BLUE	4
#define RED	5
#define GREEN	6
#define ORANGE	7
#define GREY	8
#define MAGENTA 9
#define AQUA    10
#define INDIGO	11
#define VIOLET	12
#define BROWN	13

#define RIGHT	1
#define LEFT	2

/* These are from mode.h which is no longer supported w/ digit 3.0 */
/*				 ^     for the most part...        */
/*     and definately gone by 4.0 				   */
#define POINT	0
#define STREAM	1

#define POINTS		3	/* this is thrown in for get_type_cnt() */

#define LINE		0x01
#define AREA		0x02
#define DOT		0x04


#define DEAD_LINE	0x10
#define DEAD_AREA	0x20
#define DEAD_DOT	0x40

#define FILE_LINE		0
#define FILE_AREA		1
#define FILE_DOT		2

#define FILE_DEAD_LINE		4
#define FILE_DEAD_AREA		5
#define FILE_DEAD_DOT		6


#define ESC	033

#ifdef FOO
/* these have all turned into global ints.  in digit/globals.h */
/* colors of unique items on screen */
#define CLR_LINE	BLUE
#define CLR_AREA	GREY
#define CLR_DOT		GREEN
#define CLR_SITE	GREEN

#define CLR_LLINE	MAGENTA
#define CLR_LSITE	AQUA
#define CLR_LAREA	ORANGE

#define CLR_AMARK	AQUA
#define CLR_ALABEL	ORANGE
#define CLR_LLABEL	MAGENTA

#define CLR_HIGHLIGHT	YELLOW
#define CLR_ERASE	BLACK
#define CLR_UNKNOWN	WHITE
#define CLR_OVERLAY	WHITE

#define CLR_0_NODE	GREEN
#define CLR_1_NODE	ORANGE
#define CLR_2_NODE	RED
#endif
