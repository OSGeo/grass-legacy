/**** macros.h ****/
/*
**  Written by David Gerdes  Fall 1992
**  US Army Construction Engineering Research Lab
**  Copyright  David Gerdes  USA-CERL  1992
*/



#define ADD_NEW_TABLE_ENTRY  					      \
								      \
    TP = table_new (Table);                                           \
    TP->inter = next_intersection;                                    \
    TP->x = x; 	TP->y = y;                                            \
    TP->used = 0;                                                     \
    TP->in_out = FLAG;                                                \
                                                                      \
    if (aarea > 0)                                                    \
    {                                                                 \
	TP->i[A_CODE].poly = aarea;                                   \
	MapA->Area[aarea].alive = POLY_INTERSECT;                     \
    }                                                                 \
    else				/* island */                  \
    {                                                                 \
	TP->i[A_CODE].poly = MapA->Isle[-aarea].area;                 \
	MapA->Isle[-aarea].alive = POLY_INTERSECT;                    \
    }                                                                 \
                                                                      \
                                                                      \
    TP->i[A_CODE].subpoly = A_subpoly;                                \
    TP->i[A_CODE].line = get_area_line_pos (MapA, aarea, aline);      \
    TP->i[A_CODE].l_index = aline * Adir;                             \
    TP->i[A_CODE].dir     = Adir;                                     \
    TP->i[A_CODE].vertex = VERTEX;                                    \
                                                                      \
    if (Adir == FORWARD)                                              \
      TP->i[A_CODE].segment = a;                                      \
    else                                                              \
      TP->i[A_CODE].segment = APoints->n_points - a; /* 0 -> N */     \
                                                                      \
                                                                      \
                                                                      \
    if (barea > 0)                                                    \
    {                                                                 \
	TP->i[B_CODE].poly = barea;                                   \
	MapB->Area[barea].alive = POLY_INTERSECT;                     \
    }                                                                 \
    else				/* island */                  \
    {                                                                 \
	TP->i[B_CODE].poly = MapB->Isle[-barea].area;                 \
	MapB->Isle[-barea].alive = POLY_INTERSECT;                    \
    }                                                                 \
                                                                      \
    TP->i[B_CODE].subpoly = B_subpoly;                                \
    TP->i[B_CODE].line = get_area_line_pos (MapB, barea, bline);      \
    TP->i[B_CODE].l_index = bline * Bdir;                             \
    TP->i[B_CODE].dir     = Bdir;                                     \
    TP->i[B_CODE].vertex = VERTEX;                                    \
    if (Bdir == FORWARD)                                              \
      TP->i[B_CODE].segment = b; /* 0 -> N */                         \
    else                                                              \
      TP->i[B_CODE].segment = BPoints->n_points - b; /* 0 -> N */     







#define ADD_NEW_TABLE_LINE_ENTRY 				      \
								      \
    TP = table_new (Table);                                           \
    TP->inter = next_intersection;                                    \
    TP->x = x; 	TP->y = y;                                            \
    TP->used = 0;                                                     \
    TP->in_out = FLAG;                                                \
                                                                      \
    if (aarea > 0)                                                    \
    {                                                                 \
	TP->i[A_CODE].poly = aarea;                                   \
    }                                                                 \
    else				/* island */                  \
    {                                                                 \
	TP->i[A_CODE].poly = MapA->Isle[-aarea].area;                 \
    }                                                                 \
                                                                      \
                                                                      \
    TP->i[A_CODE].subpoly = A_subpoly;                                \
    TP->i[A_CODE].line = get_area_line_pos (MapA, aarea, aline);      \
    TP->i[A_CODE].l_index = aline * Adir;                             \
    TP->i[A_CODE].dir     = Adir;                                     \
    TP->i[A_CODE].vertex = VERTEX;                                    \
                                                                      \
    if (Adir == FORWARD)                                              \
      TP->i[A_CODE].segment = a;                                      \
    else                                                              \
      TP->i[A_CODE].segment = APoints->n_points - a; /* 0 -> N */     \
                                                                      \
                                                                      \
                                                                      \
    TP->i[B_CODE].poly = 0;		                              \
    *((float *) &(TP->i[B_CODE].subpoly)) = dist;                     \
    TP->i[B_CODE].line = 0;					      \
    TP->i[B_CODE].l_index = bline * Bdir;                             \
    TP->i[B_CODE].dir     = Bdir;                                     \
    TP->i[B_CODE].vertex = VERTEX;                                    \
    if (Bdir == FORWARD)                                              \
      TP->i[B_CODE].segment = b; /* 0 -> N */                         \
    else                                                              \
      TP->i[B_CODE].segment = BPoints->n_points - b; /* 0 -> N */     





