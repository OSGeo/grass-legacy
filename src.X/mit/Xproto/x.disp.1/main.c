

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xmu/Xmu.h>

Widget top_level;

extern void Destroyed();


main(argc, argv)
int argc;
char *argv[];

{
    extern void create_main_menu();

    printf("\n ola");

    printf("\n haha");

    /* X toolkit initialization and creation     */
    /* of the top level widget           */
    top_level = XtInitialize("graphics",
        "Demo",
        "NULL",
        0,
        &argc, argv);

    /* XtAddCallback(top_level, XtNdestroyCallback, Destroyed,
     * NULL); */


    set_display_args();

    printf("\n thank_god");


    create_main_menu();


    printf("\n yuck0");
    XtRealizeWidget(top_level);

    XtMainLoop();

}
