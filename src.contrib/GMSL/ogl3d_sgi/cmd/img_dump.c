#include "interface.h"

char *Last_dir=NULL;

void
pop_imgdump(w, dc, call_data)
Widget w;
data_cell *dc;
XmAnyCallbackStruct call_data;
{
Widget fsbox;
   
    dc->fsbox = make_fsbox (dc->form_for_main_control_panel, "Select File", 
	"name of rgb file for saved image", "*.tga", Last_dir, save_img, dc);
    XtManageChild(dc->fsbox);
}

void
save_img(w, dc, call_data)
Widget w;
data_cell *dc;
XmFileSelectionBoxCallbackStruct *call_data;
{
char *filename, report[160];
static int first = 1;
  
    if(first){
        first = 0;
	Last_dir = (char *)malloc (sizeof (char) * 128);
    }
    XmStringGetLtoR (call_data->dir, XmSTRING_DEFAULT_CHARSET, &Last_dir);

    filename = (char *)malloc (sizeof (char) * 128);
    XmStringGetLtoR (call_data->value, XmSTRING_DEFAULT_CHARSET, &filename);

    XtDestroyWidget(dc->fsbox);

    sprintf(report,"Saving %s...", filename);
    inform(dc, report);
    
    if(filename[0] != '\0'){
	 if(0 > targa_out(filename)){
	    sprintf(report,"Unable to save %s", filename);
	    XBell(XtDisplay(w), 50);
	}
	else
	
	    sprintf(report,"%s saved.", filename);
    }
    else
	sprintf(report,"<request cancelled>");

    inform(dc, report);
    free(filename);
}




