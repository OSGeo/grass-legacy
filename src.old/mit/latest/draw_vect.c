#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/List.h>
extern Window  display_win;
extern int n_vects, list_vect_flag;
extern char *vector_files[5];

void draw_vectors(new, client_data, call_data)
	Widget new;
	caddr_t client_data;
	caddr_t call_data;
{
	char buf[100];
	XtListReturnStruct *vect;

	XtUnmapWidget(XtParent(new));
	list_vect_flag = 0;
	vect = (XtListReturnStruct *) call_data;	
	
	
	printf("\n vectors drawn");
	sprintf(buf, "Xvect %u %s", 
		display_win,  vect->string);
	system(buf);


	vector_files[n_vects] = (char *) malloc(25);
	strcpy(vector_files[n_vects], vect->string);

	n_vects++;




}
