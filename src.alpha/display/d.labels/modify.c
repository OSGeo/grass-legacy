#include "config.h"
modify()
{
    int i;
    char num[10];

    sprintf (num,"[%d]", config.count+1);

    V_const (num, 's', 1, 70, 7);
    V_ques (reset_loc,    's',RESET,17,sizeof(reset_loc) - 1);
    V_ques (config.east,   's',EAST, 17,sizeof(config.east) - 1);
    V_ques (config.xoffset,'s',EAST, 46,sizeof(config.xoffset) - 1);
    V_ques (config.north,  's',NORTH,17,sizeof(config.north) - 1);
    V_ques (config.yoffset,'s',NORTH,46,sizeof(config.yoffset) - 1);
    V_ques (config.ref,    's',REF,17,sizeof(config.ref) - 1);

    for (i = 0; i < TEXTLINES; i++)
	V_ques (config.text[i],'s',TEXT+i,6,sizeof(config.text[i])-1);
    V_ques (config.skip,    's',TEXT,  66, sizeof (config.skip) - 1);

    V_ques (config.font,   's',FONT, 19,sizeof(config.font) - 1);
    V_ques (config.size,   's',SIZE, 19,sizeof(config.size) - 1);
    V_ques (config.color,  's',COLOR,19,sizeof(config.color) - 1);
    V_ques (config.width,   's',COLOR, 47,sizeof(config.width) - 1);
    V_ques (config.hcolor,  's',HCOLOR, 19,sizeof(config.hcolor) - 1);
    V_ques (config.hwidth,   's',HCOLOR, 47,sizeof(config.hwidth) - 1);
    V_ques (config.background,'s',BACKGROUND,19,sizeof(config.background) - 1);
    V_ques (config.border,  's',BORDER,19, sizeof(config.border) - 1);
    V_ques (config.opaque,  's',OPAQUE,19, sizeof(config.opaque) - 1);

    V_intrpt_ok();
    return V_call();
}
