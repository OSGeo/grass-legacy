#define MAIN
#include <stdio.h>
#include <string.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/DrawingA.h>
#include <Xm/DrawnB.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/Label.h>
#include <Xm/List.h>
#include <Xm/MessageB.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <Xm/ScrolledW.h>
#include <Xm/ScrollBar.h>
#include <Xm/SelectioB.h>
#include <Xm/Separator.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/ToggleB.h>
#include "gis.h"
#include "digit.h"
#include "gdbmi.h"
#include "global.h"

#define NCOLS 10
Widget          mainShell;
Widget          QuitButton, EditAreaAttButton, 
		EditLineAttButton, ChooseThemeButton, EditDBButton;
XtAppContext    appContext;
Display        *display;
char          **values;
void          Create_Edit_Dialog();
void            CallEditAtt(), CallEditDB(), CallTheme(), CallExit();
char           *namestring = "helvb12", *zonestring = "helvb14";

static XrmOptionDescRec initTable[] = {
{"-title",	"*title",	XrmoptionSepArg, (caddr_t)"d.vect.db Table Editor"},
{"-font",	"*fontList",	XrmoptionSepArg, (caddr_t)"fixed"},
{"-fn",		"*fontList",	XrmoptionSepArg, (caddr_t)"fixed"},
};

char *name, *mapset, *openvect();
struct Map_info Map;
struct Categories Cats;
int level;
int color, hilite_color;

main(argc, argv)
    int             argc;
    char           **argv;
{
    Widget          area, dialog;
    Arg             al[30];
    int             ac;
    struct Option *opt1, *opt2, *opt3;
    char *D_color_list();

	/* Initialize the GIS calls */
	G_gisinit (argv[0]) ;

	opt1 = G_define_option() ;
	opt1->key        = "map" ;
	opt1->type       = TYPE_STRING ;
	opt1->required   = YES ;
	opt1->gisprompt  = "old,dig,vector" ;
	opt1->description= "Name of existing vector map" ;

	opt2 = G_define_option() ;
	opt2->key        = "color" ;
	opt2->type       = TYPE_STRING ;
	opt2->answer     = "white" ;
	opt2->options    = D_color_list();
	opt2->description= "Color desired for drawing map" ;

	opt3 = G_define_option() ;
	opt3->key        = "hcolor" ;
	opt3->type       = TYPE_STRING ;
	opt3->answer     = "yellow" ;
	opt3->options    = D_color_list();
	opt3->description= "Color desired for highlighting map objects" ;

	if(G_parser(argc,argv))
		exit(1);

        color = D_translate_color(opt2->answer);
	hilite_color = D_translate_color(opt3->answer);
	name = opt1->answer;

	/* Look at maps given on command line */

	mapset = openvect (name);
	if (mapset == NULL)
	{
		fprintf (stderr, "Unable to open %s\n", name) ;
		exit(1) ;
	}
	R_open_driver();
	D_setup(0);

	level = Vect_open_old (&Map, name, mapset);
	if (level < 0)
		G_fatal_error ("Can't open vector file");
	;
	if (level < 2)
		G_fatal_error ("You must first run v.support on vector file");

	if (G_read_vector_cats(name, mapset, &Cats) < 0)
		Cats.num = -1  ;

        /* draw the map */
	hilite(Map, 0, color, hilite_color);


    mainShell = shell = XtAppInitialize(&appContext, "d.vect.edit",
					initTable, XtNumber(initTable),
					&argc, argv, NULL, NULL, 0);

    display = XtDisplay(shell);
    A.AttTable =NULL;
    B.AttTable =NULL;
    C.AttTable =NULL;
    A.AttTableManaged =0;
    B.AttTableManaged =0;
    C.AttTableManaged =0;

    dialog = XmCreateRowColumn(shell, "main message", NULL, 0);
    EditLineAttButton = XmCreatePushButton(dialog, "View/Edit Attributes of lines", NULL, 0);
    XtAddCallback(EditLineAttButton, XmNactivateCallback, CallEditAtt, LINE_TYPE);
    XtManageChild(EditLineAttButton);

    EditAreaAttButton = XmCreatePushButton(dialog, "View/Edit Attributes of areas/polygons", NULL, 0);
    XtAddCallback(EditAreaAttButton, XmNactivateCallback, CallEditAtt, AREA_TYPE);
    XtManageChild(EditAreaAttButton);

    ChooseThemeButton = XmCreatePushButton(dialog, "Specify New/Existing Theme", NULL, 0);
    XtAddCallback(ChooseThemeButton, XmNactivateCallback, CallTheme, NULL);
    XtManageChild(ChooseThemeButton);

    EditDBButton = XmCreatePushButton(dialog, "Edit the Data Base", NULL, 0);
    XtAddCallback(EditDBButton, XmNactivateCallback, CallEditDB, NULL);
    XtManageChild(EditDBButton);

    QuitButton = XmCreatePushButton(dialog, "Exit", NULL, 0);
    XtAddCallback(QuitButton, XmNactivateCallback, CallExit, NULL);
    XtManageChild(QuitButton);

    XtManageChild(dialog);
    
    XtRealizeWidget(shell);
    XtAppMainLoop(appContext);
    R_close_driver();
    Vect_close (&Map);
}

void CallEditAtt(w, type, data)
     Widget w;
     caddr_t data;
     int type;
{
     static int cat=0;
     static char buf[10];

     /* first unhighlight previous choice */
     if(cat)
     {
        hilite(Map, 0, color, hilite_color);
	cat = 0;
     }
     /* choose vect id  to show attributes for */
     cat = what(&Map, &Cats, color, hilite_color, type); 
     if(cat==0)
     {
       printf("No object selected!\n");
       return;
     }
	
     A.AttTableNumCols = 10;
     A.AttValues = (char **) G_malloc(10 * sizeof(char *));
     A.AttValues[0] = "1";
     A.AttValues[1] = "2";
     A.AttValues[2] = "2";
     A.AttValues[3] = "3";
     A.AttValues[4] = "4";
     A.AttValues[5] = "5";
     A.AttValues[6] = "8";
     A.AttValues[7] = "8";
     A.AttValues[8] = "xxxxxxxx";
     A.AttValues[9] = "olga";
     A.nalloc = 0;
     A.AttTableName = G_store("Table A");
     A.AttKeys = (char **) G_malloc(1 * sizeof(char *));
     sprintf(buf, "%d", cat * 2);
     A.AttKeys[0] = buf;
     Create_Edit_Dialog(shell, &A);
     XtManageChild(A.AttTable);
     A.AttTableManaged = 1;
}

void CallExit(w, call_data, arg)
     Widget w;
     caddr_t call_data, arg;

{
    R_close_driver();
    Vect_close (&Map);
    exit(0);
}

void CallEditDB(w, call_data, arg)
     Widget w;
     caddr_t call_data, arg;
{
}

void CallTheme(w, call_data, arg)
     Widget w;
     caddr_t call_data, arg;
{
}

