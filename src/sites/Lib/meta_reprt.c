meta_report (report_file, site_file, layer_file, quadsize, with_stats)

	char *report_file;
	char *site_file;
	char *layer_file;
{
	char command[300];

	sprintf (command, "meta_report %s %s %s %d %c\n",
		report_file, site_file, layer_file,
		quadsize, with_stats ? 'T' : 'F');
	
	return execute (command);
}
