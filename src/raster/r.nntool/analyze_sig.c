
#include "globals.h"

analyze_sig()
{
  int nbands, save_signature(), j, button, x, y;
  extern int numfiles;

  Menu_msg("");

  /* allocate row buffers and open cell files */
  G_set_window (&Band_cellhd);
  nbands = numfiles;
  open_band_files();

  /* gather all points which fall within the polygon(s)! */
  /* perform a signature of the area */

  signalflag.interrupt = 0;
/* for(Uj=0;Uj < SA;Uj++) { */
  if(!outline()) return(-1);
  if (!prepare_signature(nbands)) {
    close_band_files();
    return(-1);
  }

  show_signature (nbands, 1.5);
  save_signature();
/*  if(SA > 1) {
	use_mouse_msg();
	Menu_msg("Hit any button for next");
	Mouse_pointer(&x,&y,&button);
	if(button==RIGHT_BUTTON||button==LEFT_BUTTON||button==MIDDLE_BUTTON);
  }
} */

 close_band_files();
 done:
/* clean up after analysis */
  if (Region.perimeter)
    {
      free (Region.perimeter);
      Region.perimeter = 0;
    }
  erase_region();
  Menu_msg("");

  return (0);
}
