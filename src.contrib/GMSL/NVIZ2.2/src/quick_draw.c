/* quick_draw:
 ** uses libgsf to draw wire frame surfaces
 */
#include <stdlib.h>
#include "tk.h"
#include "interface.h" 

int Nquick_draw_cmd (Nv_data *dc, Tcl_Interp *interp)
{
  int i, max;
  int *surf_list;

  GS_set_draw(GSD_BACK);
  GS_clear(dc->BGcolor);
  GS_ready_draw();
  surf_list = GS_get_surf_list(&max);

  max = GS_num_surfs();
  for(i=0; i<max; i++) {
    if (check_blank(interp,surf_list[i]) == 0) {
      GS_draw_wire(surf_list[i]);
    }
  }

  
  GS_done_draw();
  free(surf_list);

  return (TCL_OK);
}
