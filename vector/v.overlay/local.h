#define OP_AND 0
#define OP_OR  1
#define OP_NOT 2
#define OP_XOR 3

#define SEP "-----------------------------------------------------------------------------\n"

typedef struct {
    double x, y;
    struct line_cats *cat[2]; /* category in map a and b */
    char valid; 
} CENTR; 

int area_area ( struct Map_info *In, int *field, struct Map_info *Out, struct field_info *Fi,
	                dbDriver *driver, int operator  );
int line_area ( struct Map_info *In, int *field, struct Map_info *Out, struct field_info *Fi,
	                dbDriver *driver, int operator  );
