/* Include file defining structures used in gshhs.c */

struct GSHHS {	/* Global Self-consistant Hierarchical High-resolution Shorelines */
	int id;				/* Unique polygon id number, starting at 0 */
	int n;				/* Number of points in this polygon */
	int level;			/* 1 land, 2 lake, 3 island_in_lake, 4 pond_in_island_in_lake */
	int west, east, south, north;	/* min/max extent in micro-degrees */
	int area;			/* Area of polygon in 1/10 km^2 */
	short int greenwich;		/* Greenwich is 1 if Greenwich is crossed */
	short int source;		/* 0 = CIA WDBII, 1 = WVS */
};

struct	POINT {
	int	x;
	int	y;
};
