#include "global.h"

int 
alloc_list (void)
{

    switch (options.list) {
        case LIST_CI:
            list_ci = (CI *)G_malloc(vstat.alloc * sizeof(CI));
            break;
        case LIST_CD: 
	    list_cd = (CD *)G_malloc(vstat.alloc * sizeof(CD));
            break;
	case LIST_CI2D:
            list_ci2d = (CI2D *)G_malloc(vstat.alloc * sizeof(CI2D));
            break;
        case LIST_CC:
            list_cc = (CC *)G_malloc(vstat.alloc * sizeof(CC));
            break;
    }  					    

    return 0;
}

int 
free_list (void)
{
    switch (options.list) {
        case LIST_CI:
            G_free(list_ci);
            break;
        case LIST_CD:
            G_free(list_cd);
            break;
        case LIST_CI2D:
            G_free(list_ci2d);
            break;
        case LIST_CC:
            G_free(list_cc);
            break;
    } 
    return 0;
}
