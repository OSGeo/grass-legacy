
/* lgtexp:
** standard expose callback. this one is for the puck.
*/

#include "interface.h"


void 
lgtexp(w, dc, call_data)
    Widget w;
    data_cell *dc;
    caddr_t call_data;
{
    trackmouse(w, dc, NULL);
}
