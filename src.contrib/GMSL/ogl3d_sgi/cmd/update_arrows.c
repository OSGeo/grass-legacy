
/* update_arrows:
** updates the text for the arrow button labels when the
** arrow buttons are pressed
*/

#include "interface.h"

#define MAXMOST 100
#define MINALL 1
#define MAXLINE 15




void
update_arrows (w, ap, call_data)
Widget w; 
arrow_pair *ap;
XmArrowButtonCallbackStruct    *call_data;
{
    int found = 0, i;
    char   str[80];


    for(i = 0; i < MAX_ARROWS_N; i++) {

	if(w == ap->up) {
	    found = 1;
	    ap->val++;
	}
	else if(w == ap->down) {
	    found = 1;
	    ap->val--;
	}

	if(found){

	    if(ap->val > ap->max) 
		ap->val = ap->max;
	    
	    if(ap->val < ap->min) 
		ap->val = ap->min;

	    sprintf(str, "%d", ap->val);

	    change_label(ap->txt, str);
    
	    return;
	}

    } /*-- end of for i --*/

}


