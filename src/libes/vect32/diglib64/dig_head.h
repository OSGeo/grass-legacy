#ifndef  GRASS_DIGHEAD_H
#define  GRASS_DIGHEAD_H

#define DIG_ORGAN_LEN       30
#define DIG_DATE_LEN        20
#define DIG_YOUR_NAME_LEN   20
#define DIG_MAP_NAME_LEN    41
#define DIG_SOURCE_DATE_LEN 11
#define DIG_LINE_3_LEN      53	/* see below */

#define OLD_LINE_3_SIZE 73
#define NEW_LINE_3_SIZE 53
#define VERS_4_DATA_SIZE 20


struct dig_head
{
	    /******** Original info ******/
    char organization[30] ;
    char date[20] ;
    char your_name[20] ;
    char map_name[41] ;
    char source_date[11] ;
    long  orig_scale ;
    char line_3[73] ;
    int plani_zone ;
    double W, E, S, N ;
    double digit_thresh ;
    double map_thresh ;

	    /* Programmers should NOT touch any thing below here */
	    /* Library takes care of everything for you          */
	    /**********New 4.0************/
    int Version_Major;
    int Version_Minor;
    int Back_Major;
    int Back_Minor;

    /* portability stuff */
    int portable;

    struct Map_info *Map;	/* X-ref to Map_info struct ?? */
};

#endif
