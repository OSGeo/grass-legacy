#include "tape.h"

ask_title ()
{
  int repeat;
  int len;
  int bufsize;
  char tmp[128];
  char *buf;

  buf = "TM Imagery File Extracted From Tape";
  len = strlen(buf);
  repeat = 1;
  do {
    V_clear ();
    V_line (1, "please type in the title");

    V_line (3,"TITLE FOR THE EXTRACTED IMAGERY");
    V_const (buf, 's', 5, 3, len+10);
    V_ques (buf, 's', 5, 3, len+10);

    I_v_exec();
    if (*buf != 0)
      repeat = 0;
  } while (repeat);

  sprintf (tmp, "\"%s\"", buf);
  G_strip(buf);
  bufsize = strlen(buf);
  title = G_malloc(bufsize+1);
  G_strcpy(title,tmp);
}
