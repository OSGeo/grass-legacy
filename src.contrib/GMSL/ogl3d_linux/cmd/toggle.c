

/* toggle:
** checks to see which button was pressed, sets the button in the toggle array,
** and calls the necessary response function
*/


#include "interface.h"

void 
toggle (Widget w, data_cell *dc, caddr_t ignored)
{

    int i;
    char *str;	

    for(i = 0; i < MAX_TOGGLES; ++i) {
	if(w == dc->toggle_id[i]) {
/*	    
-----------removed the variables structure from dc in Interface.h It didn't--------------
-------------look necessary to me should make dc->vars.whatever unnecessary(E. Cline 1997)--
*/
            dc->toggles[i] = !dc->toggles[i];
	    fprintf (stdout," i am dc->toggles[i] and i = %d\n",
				    dc->toggles[i]);

	    if(dc->toggles[i])
	      sprintf(str, "%s %d %s","i am widget #", w,"and i was set");
	    else
	      sprintf(str, "%s %d %s","i am widget #", w,"and i was unset");

	    inform(dc,str);
	}
    }	
		
}


