dump_dlg(dlg) struct dlg *dlg;
{
    int i ;
    printf ("max_lines %d\n", dlg->max_lines);
    for (i=1; i <= dlg->max_lines; i++)
	printf ("%5ld%s",dlg->line_off[i],i%10?" ":"\n");
    printf ("\n");
}
