#include "Gwater.h"
#define BIGNEG	-9999999

overland_cells (row,col,basin_num, haf_num, hih_ele)
int row, col;
CELL basin_num, haf_num, *hih_ele;
{
    int r, rr, c, cc;
    CELL new_ele, new_max_ele, value;

    bas[SEG_INDEX(bas_seg,row,col)] = basin_num;
    haf[SEG_INDEX(haf_seg,row,col)] = haf_num;
    new_max_ele = BIGNEG;
    for (r=row-1, rr=0; r<=row+1; r++, rr++)
    {
        for (c=col-1, cc=0; c<=col+1; c++, cc++)
        {
            if (r>=0 && c>=0 && r<nrows && c<ncols)
            {
		value = asp[SEG_INDEX(asp_seg,r,c)];
                if (value == drain[rr][cc])
                {
		    if (r != row && c != col)
		    {
			overland_cells (r, c, basin_num, haf_num, &new_ele);
		    }
		    else if (r != row)
		    {
			overland_cells (r, c, basin_num, haf_num, &new_ele);
		    }
		    else
		    {
			overland_cells (r, c, basin_num, haf_num, &new_ele);
		    }
                }
            }
        }
    }
    if (new_max_ele == BIGNEG)
    {
	*hih_ele = alt[SEG_INDEX(alt_seg,row,col)];
    }
    else
    {
	*hih_ele = new_max_ele;
    }
}
