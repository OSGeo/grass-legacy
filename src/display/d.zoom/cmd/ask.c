#include <stdio.h>
#include "display.h"
#include "local_proto.h"

int yes (char *prompt)
{
    char *text[4];

    text[0] = prompt;
    text[1] = "YES";
    text[2] = "NO";
    text[3] = (char *)NULL;

    return (1==ask(text));
}

int just_click (char *msg)
{
    char *text[3];

    text[0] = msg;
    text[1] = "Click here to continue";
    text[2] = (char *)NULL;

    ask(text);

    return 0;
}

int 
ask (char *text[])
{
    return (D_popup (
	D_translate_color("red"),
	D_translate_color("white"),
	D_translate_color("white"),
	0,0,3,text));
}

int 
ask_rotate (void)
{
    char *text[4];

    text[0] = "Select the type of \"zoom\"";
    text[1] = "Zoom using a rubberband box";
    text[2] = "Rotate the world by selecting center longitude";
    text[3] = (char *) NULL;

    return (2==ask(text));
}
