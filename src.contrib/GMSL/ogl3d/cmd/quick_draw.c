
/* quick_draw:
** uses libgsf to draw wire frame surfaces
*/

#include "interface.h" 

int 
quick_draw (data_cell *dc)
{
int i;

    GS_set_draw(GSD_BACK);
    GS_clear(rgb_int_from_pix(dc, dc->cells[BG_CELL]));
    GS_ready_draw();


    for(i=0; i<MAX_SURFS; i++){
	if(dc->hSurf[i])      /* and turned on? */
	    GS_draw_wire(dc->hSurf[i]);
    }

    GS_done_draw();
}

int 
cplane_draw (int num, data_cell *dc)
{
int i, j, nsurfs;
int sortSurfs[MAX_SURFS], sorti[MAX_SURFS];

    GS_set_draw(GSD_BACK);
    GS_clear(rgb_int_from_pix(dc, dc->cells[BG_CELL]));
    GS_ready_draw();

    nsurfs = GS_num_surfs();
    if(nsurfs>1){
	for (i=0; i< MAX_CPLANES; i++){
	    if(dc->Cp_on[i])
		GS_draw_cplane_fence(dc->hSurf[0], dc->hSurf[1], i);
		/* params will change - surfs bogus now */
	}
    }

    for(i=0; i<nsurfs; i++){
	if(dc->hSurf[i])      /* and turned on? */
	    GS_draw_wire(dc->hSurf[i]);
    }

    GS_done_draw();
}

