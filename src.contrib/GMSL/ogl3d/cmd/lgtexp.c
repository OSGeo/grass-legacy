
/* lgtexp:
** standard expose callback. this one is for the puck.
*/

#include "interface.h"


void 
lgtexp (Widget w, data_cell *dc, caddr_t call_data)
{
    trackmouse(w, dc, NULL);
}
