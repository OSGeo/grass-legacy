#include <grass/gis.h>
#ifdef HAVE_CURSES_H
#include <grass/vask.h>
#include <grass/imagery.h>
int I_clear_tape_info(struct Tape_Info *t)
{
    struct Tape_Info template;
    int i;

    t->title[0] = 0;

    for (i = 0; i < sizeof (template.id)/sizeof (template.id[0]); i++)
	t->id[i][0] = 0;

    for (i = 0; i < sizeof (template.desc)/sizeof (template.desc[0]); i++)
	t->desc[i][0] = 0;

    return 0;
}

int I_edit_tape_info (struct Tape_Info *t)
{
    int i;

    V_clear ();
    V_line (1,"Please enter the following information");
    V_line (3,"TAPE IDENTIFICATION");
    V_line (7,"IMAGE DESCRIPTION");
    V_line (14,"TITLE FOR THE EXTRACTED CELL FILES");

    V_ques (t->id[0], 's', 4, 3, sizeof(t->id[0])-1);
    V_ques (t->id[1], 's', 5, 3, sizeof(t->id[1])-1);

    for (i=0; i < 5; i++)
	V_ques (t->desc[i], 's', 8+i, 3, sizeof(t->desc[i])-1);
    V_ques (t->title, 's', 15, 3, sizeof(t->title)-1);

    I_v_exec();

    G_strip (t->title);
    G_strip (t->id[0]);
    G_strip (t->id[1]);
    for (i = 0; i < 5; i++)
	G_strip (t->desc[i]);

    return 0;
}
#endif
