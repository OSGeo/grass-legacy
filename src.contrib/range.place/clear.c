/* @(#)clr_scr.c	1.1   4/10/87 */

clr_scr()
{
	char **pads;
	int npads;
	int count;
	int p;

	R_pad_list (&pads, &npads);
	for (p = -1; p < npads; p++)
	{
		if (p < 0)
		{
			R_pad_select ("");
			R_pad_delete_item("time") ;
			R_pad_delete_item("cur_w") ;
		}
		else
		{
			R_pad_select (pads[p]);
			R_pad_delete ();
		}
	}

	R_standard_color(D_translate_color("black")) ;
	R_erase() ;

	R_close_driver();
}
