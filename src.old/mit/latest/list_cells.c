
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

int list_cell_flag = 0;
Widget cell_shell, cell_list;
extern void draw_map();

static XtCallbackRec draw_map_callback[] = {
		{draw_map, NULL},
		{NULL, NULL}
	};


void list_cells(cell, client_data, call_data)
	Widget cell;
	caddr_t client_data;
	caddr_t call_data;
{
	Arg arglist[20];
	extern XFontStruct *font5;
	char **list_maps, **form_cell_list();
	extern char **form_list();
	if(list_cell_flag == 1) return;
	printf("\n allo");
	XtSetArg(arglist[0], XtNiconName, "cells");

	cell_shell = XtCreateApplicationShell("cell_shell",
			topLevelShellWidgetClass,
			arglist, 1);

	list_maps = form_list("cell");	
	
	XtSetArg(arglist[0], XtNcallback,draw_map_callback);
	XtSetArg(arglist[1], XtNlist, list_maps);
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


	cell_list = XtCreateManagedWidget("celllist",
			listWidgetClass, cell_shell,
			arglist, 12);




	XtRealizeWidget(cell_shell);
	list_cell_flag = 1;
}


char **form_cell_list()
{
	FILE *ls, *popen();
	char *malloc();
	char **record;
	char path[256], buf[256];
	char *mapset, *map;
	int i=0;

	record = (char **) malloc(1000 * sizeof(char *));
	mapset = G_mapset();	
	G__file_name(path, "cell", "", mapset);
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


}	/* end of function "form_cell_list" */
