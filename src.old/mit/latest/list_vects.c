
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/List.h>
#include <X11/Shell.h>
#include <X11/Form.h>
#include <X11/Box.h>
#include "gis.h"

#define	nActions(x) 	(sizeof(x)/sizeof(XtActionsRec))

extern Display *the_display;
extern XtTranslations form_trans;
extern Cursor cur1;

int list_vect_flag = 0;
Widget vect_shell, vect_list;
extern void draw_vectors();

static XtCallbackRec draw_vectors_callback[] = {
		{draw_vectors, NULL},
		{NULL, NULL}
	};


void list_vects(vect, client_data, call_data)
	Widget vect;
	caddr_t client_data;
	caddr_t call_data;
{
	Arg arglist[20];
	extern XFontStruct *font5;
	char **list_vects, **form_list();
	if(list_vect_flag == 1) return;
	printf("\n allo");
	XtSetArg(arglist[0], XtNiconName, "vectors");

	vect_shell = XtCreateApplicationShell("vect_shell",
			topLevelShellWidgetClass,
			arglist, 1);

	list_vects = form_list("dig");	
	
	XtSetArg(arglist[0], XtNcallback,draw_vectors_callback);
	XtSetArg(arglist[1], XtNlist, list_vects);
	XtSetArg(arglist[2], XtNinternalHeight, 7);
	XtSetArg(arglist[3], XtNinternalWidth,  7);
	XtSetArg(arglist[4], XtNcolumnSpacing,  10);
	XtSetArg(arglist[5], XtNrowSpacing,  5);
	XtSetArg(arglist[6], XtNbackground, pixel("light steel blue"));
	XtSetArg(arglist[7], XtNverticalList, True);
	XtSetArg(arglist[8], XtNfont,  font5);
	XtSetArg(arglist[9], XtNborderColor, pixel("cyan"));
	XtSetArg(arglist[10], XtNborderWidth,  2);
	XtSetArg(arglist[11],XtNforeground,pixel("yellow"));


	vect_list = XtCreateManagedWidget("vectlist",
			listWidgetClass, vect_shell,
			arglist, 12);




	XtRealizeWidget(vect_shell);
	list_vect_flag = 1;
}


char **form_list(element)
	char *element;
{
	FILE *ls, *popen();
	char *malloc();
	char **record;
	char path[256], buf[256];
	char *mapset, *map;
	int i=0;

	record = (char **) malloc(1000 * sizeof(char *));
	mapset = G_mapset();	
	G__file_name(path, element, "", mapset);
	sprintf(buf, "ls %s", path);

	ls = popen(buf, "r");

	while(fgets(buf, 40, ls))
		{
		map = malloc(25);


		sscanf(buf, "%[^\n]", map);
		*(record + i) = map;	
		
		i++;
		}

	pclose(ls);	
	
	*(record +  i) = NULL;

	return(record);


}	/* end of function "form_list" */
