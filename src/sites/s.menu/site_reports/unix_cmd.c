unix_cmd (pgm, raw_report, report)
    char *pgm;
    char *raw_report;
    char *report;
{
    char temp[3];

    if (sscanf (pgm, "%1s", temp) != 1)
	    return 0;

    printf("%s\n", pgm);
    return run_report (pgm, raw_report, report);
}
