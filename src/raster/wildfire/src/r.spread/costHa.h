/*************************************************
 *
 *   costHa.h - Data structure for costHa (spread)
 *
 *   It is used for (heap) sorting the
 *   cumulative time, it's a contiguous structure.
 *
 *************************************************/

struct costHa {
	float           min_cost, angle;
	int             row, col;
};
