#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Form.h>
#include <X11/List.h>


Widget top_level, f, l;
Display *the_display;
int the_screen;



main(argc, argv)
	int argc;
	char *argv[];
{
	Arg arglist[10];
	String *ptr[2];
	char str[10]; int i;


	/* X toolkit initialization and creation	*/ 
	/* of the top level widget			*/
	top_level = XtInitialize("graphics",
				 "Demo",
				 "NULL",
				 0,
				 &argc, argv);

	the_display = XtDisplay(top_level);
	the_screen  = DefaultScreen(the_display);
	printf("\n uck");

	sprintf(str, "taco");
 	i= strlen(str);
	printf("\n i= %d", i);	
	str[i] = '\0';	

	XtSetArg(arglist[0], XtNheight, 150);
	XtSetArg(arglist[1], XtNwidth,  225);

	 f = XtCreateManagedWidget("cell_form",
			formWidgetClass, top_level,
			arglist, 2);
	printf("\nloco");

	ptr[0] = (String *) &str[0];	 
	ptr[1] = NULL;		 

	XtSetArg(arglist[0],XtNlist,(char **)&ptr[0]);



	l = XtCreateManagedWidget("celllist",
			listWidgetClass, f,
			arglist, 1); 

	printf("\n duck");	
	XtRealizeWidget(top_level);
	XtMainLoop();
}
