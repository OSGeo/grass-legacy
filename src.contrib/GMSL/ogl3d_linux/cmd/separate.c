
/* separate:
** inserts a string of equal signs to seperate data generated
** by the "whats here" panel.
*/

#include "interface.h"

void 
separate (Widget w, data_cell *dc, caddr_t client_data)
{

    char str[80], *str2;


    strcpy(str , "==========================================\n");

    str2 = str;

    XmStringCreate(str2, XmSTRING_DEFAULT_CHARSET);

    XmTextInsert(dc->Wtxt, dc->current_position, str2);
    dc->current_position += strlen(str2);
    XmTextShowPosition(dc->Wtxt, dc->current_position);
	
}




