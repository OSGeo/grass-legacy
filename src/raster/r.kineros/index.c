#include "define.h"

int index(nn,flag)

int nn;
int flag;
{
    int ii;
    int base;

    extern int min_ele;

/*
 *  For most applications, base should be zero (2 - 2).
 */
    base = min_ele - BASE;
/*
 *  Stream option:
 */
    if(flag == STREAM) {
        if(nn < 2)
	    ii = 0;
	else
            ii = 3*(nn - base)/2;
    }
/*
 *  Plane element option:
 */
    else if(flag == PLANE) {
	if(nn < 1)
	    ii = 0;
	else if((nn % 2) == 0)
            ii = 3*(nn - base)/2 - 1;
	else
            ii = 3*(nn + 1 - base)/2 - 2;
    }
    else {
	printf("\n ERROR: index flag not specified");
	exit(0);
    }

    return(ii);
}
