/* %W% %G% */
/* @(#)main.c	1.3   01/89 */
/* Program: d.savescreen		Paul W. Carlson	, SCS
 */

main()
{
    char name[80];

    sprintf(name, "/tmp/screen%d", getpid());
    G_gisinit("d.savescreen");
    R_open_driver();
    printf("\n\n Saving display image in file: %s\n", name);
    printf("Please wait...\n");
    R_panel_save(name, 0, 0, 0, 0);
    R_close_driver();
}
