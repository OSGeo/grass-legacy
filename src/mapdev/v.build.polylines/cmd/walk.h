#include "Vect.h"
#include "line_coords.h"

plus_t walk_back (struct Map_info*, plus_t);
plus_t walk_forward_and_pick_up_coords (struct Map_info*, plus_t, 
					struct line_pnts*, 
					struct line_coords*,
					plus_t*,
					char*);
