
#define GET_VAL 0
#define SET_VAL 1

typedef enum {
  ATT_NONE,
  ATT_CONSTANT,
  ATT_ROWS,
  ATT_COLS,
} AttributeType;

struct grid_description
{
    int    num_rows ;
    int    num_cols ;
    int    num_vect_rows ;
    int    num_vect_cols ;
    double length ;           /*  distance to shift to the east  */
    double width ;           /*  distance to shift to the north  */
    double origin_x ;        /*  lower left point of grid  */
    double origin_y ;
    double angle ;
};



struct box
{
    int    top ;
    int    bottom ;
    int    left ;
    int    right ;
};


