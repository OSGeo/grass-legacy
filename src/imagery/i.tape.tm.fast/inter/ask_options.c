#include "tape.h"

ask_options()
{
  char *prompt;

  verbose = 0;

  G_clear_screen();

  prompt = "Enter device name: ";
  if (!I_ask (prompt, inputname, 0))
    exit(1);

  prompt = "Enter bands wanted to be extracted: ";
  wantbands = I_ask_bands (THEMATIC_MAPPER_NBANDS);
  G_strcpy(groupname, I_bandname_prefix());

  fprintf (stderr, "\nThe default window is the whole imagery.\n");
  prompt = "\nWould you like to specify your extract window? ";
  if (G_yes (prompt, 1))
    ask_window();
  else
    rows = cols = 0;

  fprintf (stderr, "\nThe default title for the image is:\n\n");
  fprintf (stderr, "		TM Imagery File Extracted From Tape\n");
  prompt = "\nWould you like to change the title? ";
  if (G_yes (prompt, 1))
    ask_title();

  prompt = "\nWould you like to run in the background? ";
  if (!G_yes(prompt, 0))
    verbose ++;
}
