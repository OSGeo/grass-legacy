#include "dba_imagery.h"
#include <curses.h>

/* this is a block structure */
typedef struct
{
    char   name[50];
    struct Block_Image_Group_Ref block_ref;
    struct Camera_File_Ref camera;
    /* struct Block_Control_Points block_con_points;*/
    /* struct Block_Camera_File_Ref camera;*/
} Block;







