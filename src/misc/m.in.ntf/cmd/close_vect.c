/*****************************************************************************/
/***                                                                       ***/
/***                            close_vect()                               ***/
/***   	            Closes down all opened vector files.  		   ***/
/***               Jo Wood, Project ASSIST, 12 June 1993                   ***/
/***                                                                       ***/
/*****************************************************************************/

#include "ntf_in.h"


close_vect()
{
    
    if (O_spot == TRUE)
    {
	vect_info.dig_fp = fptr_spot;
        if (vect_info.mode == MODE_WRITE || vect_info.mode == MODE_RW)
        	Vect__write_head_binary (&vect_info, &(vect_info.head));
	fclose(fptr_spot);
	fclose(fptr_spot_att);

	G_free_cats(&cats);			/* Just save category title */
    	G_init_cats((CELL)0,"Elevation",&cats);
    	
	O_spot = FALSE;
    }

    if (O_cont == TRUE)
    {
	vect_info.dig_fp = fptr_cont;
        if (vect_info.mode == MODE_WRITE || vect_info.mode == MODE_RW)
        	Vect__write_head_binary (&vect_info, &(vect_info.head));
	fclose(fptr_cont);
	fclose(fptr_cont_att);

	G_free_cats(&cats);			/* Just save category title */
    	G_init_cats((CELL)0,"Elevation",&cats);

	O_cont = FALSE;
    }

    if (O_lake == TRUE)
    {
	vect_info.dig_fp = fptr_lake;
        if (vect_info.mode == MODE_WRITE || vect_info.mode == MODE_RW)
        	Vect__write_head_binary (&vect_info, &(vect_info.head));
	fclose(fptr_lake);
	fclose(fptr_lake_att);

	G_free_cats(&cats);			/* Just save category title */
    	G_init_cats((CELL)0,"Elevation",&cats);

	O_lake = FALSE;
    }

    if (O_break == TRUE)
    {
	vect_info.dig_fp = fptr_break;
        if (vect_info.mode == MODE_WRITE || vect_info.mode == MODE_RW)
        	Vect__write_head_binary (&vect_info, &(vect_info.head));
	fclose(fptr_break);
	fclose(fptr_break_att);

	G_free_cats(&cats);			/* Just save category title */
    	G_init_cats((CELL)0,"Elevation",&cats);

	O_break = FALSE;
    }

    if (O_coast == TRUE)
    {
	vect_info.dig_fp = fptr_coast;
        if (vect_info.mode == MODE_WRITE || vect_info.mode == MODE_RW)
        	Vect__write_head_binary (&vect_info, &(vect_info.head));
	fclose(fptr_coast);
	fclose(fptr_coast_att);

	G_free_cats(&cats);			/* Just save category title */
    	G_init_cats((CELL)0,"Elevation",&cats);

	O_coast = FALSE;
    }

    if (O_temp == TRUE)
    {
	vect_info.dig_fp = fptr_ridge;
        if (vect_info.mode == MODE_WRITE || vect_info.mode == MODE_RW)
        	Vect__write_head_binary (&vect_info, &(vect_info.head));
	fclose(fptr_ridge);
	fclose(fptr_ridge_att);

	G_free_cats(&cats);			/* Just save category title */
    	G_init_cats((CELL)0,"Elevation",&cats);

	O_temp = FALSE;
    }

    if (O_form == TRUE)
    {
	vect_info.dig_fp = fptr_form;
        if (vect_info.mode == MODE_WRITE || vect_info.mode == MODE_RW)
        	Vect__write_head_binary (&vect_info, &(vect_info.head));
	fclose(fptr_form);
	fclose(fptr_form_att);

	G_free_cats(&cats);			/* Just save category title */
    	G_init_cats((CELL)0,"Elevation",&cats);

	O_form = FALSE;
    }


    if (O_other == TRUE)
    {
	vect_info.dig_fp = fptr_other;
        if (vect_info.mode == MODE_WRITE || vect_info.mode == MODE_RW)
        	Vect__write_head_binary (&vect_info, &(vect_info.head));
	fclose(fptr_other);
	fclose(fptr_other_att);

    	G_write_vector_cats(file_out_name,&cats);
	O_other = FALSE;
    }

    if (O_vector == TRUE)
    {
    	strcat(file_out_name,"%");	/* Change name of output files.		*/
/*	strcpy (H_organ,"Not Known                    ");
    	strcpy (H_ddate,"          ");
    	strcpy (H_mname,"Not Known                              ");
    	strcpy (H_mdate,"          ");
*/    }


}
