choose_mon()
{
    char command[256];
    char name[128];
    while(1)
    {
	printf ("select a monitor: ");
	if (!G_gets(name)) continue;
	G_strip (name);
	if (!strcmp(name, "list"))
	{
	    system ("Dstatus.mon");
	    continue;
	}
	if (*name == 0) exit(0);
	sprintf (command, "Dselect.mon %s", name);
	if (!system (command)) break;
    }
    G__read_env();
}
