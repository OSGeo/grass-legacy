

/* toggle:
** checks to see which button was pressed, sets the button in the toggle array,
** and calls the necessary response function
*/


#include "interface.h"

void
toggle(w, dc, ignored)
Widget w;
data_cell *dc;
caddr_t ignored;
{

    int i;
    char *str;	

    for(i = 0; i < MAX_TOGGLES; ++i) {
	if(w == dc->toggle_id[i]) {
	    dc->vars.toggles[i] = !dc->vars.toggles[i];
	    printf(" i am dc->vars.toggles[i] and i = %d\n",
				    dc->vars.toggles[i]);

	    if(dc->vars.toggles[i])
	      sprintf(str, "%s %d %s","i am widget #", w,"and i was set");
	    else
	      sprintf(str, "%s %d %s","i am widget #", w,"and i was unset");

	    inform(dc,str);
	}
    }			
}


