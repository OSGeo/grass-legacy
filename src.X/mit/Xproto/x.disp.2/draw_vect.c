#include <stdio.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/List.h>
extern int n_vects, list_vect_flag;
extern char *vector_files[5];

void draw_vectors(new, closure, calldata)
Widget new;
caddr_t closure, calldata;
{
    char buf[100];
    XawListReturnStruct *vect;

    XtUnmapWidget(XtParent(new));
    list_vect_flag = 0;
    vect = (XawListReturnStruct *)calldata;

    fprintf(stderr, "vectors drawn\n");
    sprintf(buf, "Xvect %s", vect->string);
    system(buf);

    vector_files[n_vects] = (char *) malloc(25);
    strcpy(vector_files[n_vects], vect->string);
    n_vects++;
}

