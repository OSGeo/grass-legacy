#include <string.h>
#include "vask.h"
#include "tape.h"
#include <stdio.h>

#define TITLE_SIZE 75

int ask_title (void)
{
  int repeat;
  int bufsize;
  char *tmp;
  char *buf;

  tmp = G_calloc(TITLE_SIZE, 1);
  buf = G_calloc(TITLE_SIZE, 1);
  if (tmp == NULL || buf == NULL)
    G_fatal_error ("Memory Allocation Failed!");
  strncpy(buf, "TM Imagery File Extracted From Tape", TITLE_SIZE - 1);
  repeat = 1;
  do {
    V_clear ();
    V_line (1, "please type in the title");

    V_line (3,"TITLE FOR THE EXTRACTED IMAGERY");
    V_const (buf, 's', 5, 3, TITLE_SIZE-1);
    V_ques (buf, 's', 5, 3, TITLE_SIZE-1);

    I_v_exec();
    if (*buf != 0)
      repeat = 0;
  } while (repeat);

  snprintf (tmp, TITLE_SIZE, "%s", buf);
  G_strip(tmp);
  title = tmp;

  /* Don't free tmp, title points to it. */
  G_free (buf);

  return 0;
}
