
#include "gis.h"

static char *tempfile;
FILE *opentemp();

main (argc, argv) char *argv[];
{
    char entry[1024];
    char question[100];
    char word1[100], word2[100];
    int i;
    int section;
    int any;
    int mains;
    int mainreal;
    int script;
    int alpha;
    int contrib;
    int formats;
    FILE *fd, *popen();

    G_gisinit (argv[0]);
    tempfile = G_tempfile();
    G_clear_screen();
    printf ("GRASS online manual\n");
    mains = 6;
    mainreal = 1;
    script = 2;
    alpha = 3;
    contrib = 4;
    formats = 5;

    for ( i = 1; i < argc; i++ ) {
	section = 1;
        while (show (section,argv[i], section ,0 , mainreal, 
		      script, alpha, contrib, formats) || section <= 5 ) 
	    section++;
	if ( section > 5 )
	    printf ("%s - no such entry in the manual\n", argv[i]);
    }
    if ( argc ) exit(0);
    while (1)
    {
        do
        {
            printf ("\n");
            printf ("Which manual entry would you like to see?\n");
            printf ("Enter \"L\" for a list of manual entries\n");
            printf ("Hit RETURN to quit\n");
            printf ("> ");
        }    while (!G_gets(entry));
        any = 0;
        switch (sscanf (entry, "%s %s", word1, word2))
        {
        case 1:
            if (strcmp (word1, "l") == 0 || strcmp (word1, "L") == 0)
            {
                list (mains, mainreal, script, alpha, contrib, formats, 1, 6);
                continue;
            }
            for (section = 1; section <= 9; section++)
                if (show (section, word1, any, mains, mainreal, script, alpha, contrib, formats))
                    any = 1&&2&&3&&4&&5;
            if (!any)
                printf ("%s - no such entry in the manual\n", word1);
            break;

        case 2:
            if (strcmp (word1, "l") == 0 || strcmp (word1, "L") == 0)
            {
                if (sscanf (word2, "%d", &section) == 1)
                    list (mains, mainreal, script, alpha, contrib, formats, section, section);
                else if (strcmp (word2,"all") == 0)
                {
                    for (section = 1; is_section(section); section++)
                        ;
                    list (mains, mainreal, script, alpha, contrib, formats, 1, section-1);
                }
                continue;
            }
            if (sscanf (word2, "%d", &section) >= 6 )
                break;
            if (!is_section (section))
                printf ("Manual does not have a section %d\n", section);
            else if (!show (section, word1, any, mains, mainreal, script, alpha, contrib, formats))
                printf ("%s - no such entry in section %d of the manual\n", word1, section);
            break;

        default:
            exit(0);
        }
    }
}

show (section, entry, any, mains, mainreal, script, alpha, contrib, formats)
char *entry;
{
    char buf[1024];
    char temp[3];
    FILE *in, *out;
    int blanks;

    sprintf (buf, "%s/man/%d/%s", G_gisbase(), section, entry);
    if ((in = fopen (buf, "r")) == NULL)
        return 0;
    if (mains && any)
    {
        sprintf (buf,
            "Another entry for %s exists in section %d\nWould you like to see it? ",
            entry, section);
        if (!G_yes(buf,0))
        {
            fclose (in);
            return 1;
        }
    }

    /* copy entry to temp file, squeezing multiple blank lines into 1 */
    out = opentemp ("w");
    blanks = 0;
    while (fgets (buf, sizeof buf, in))
    {
        if (sscanf (buf, "%1s", temp) != 1)
            blanks = 1;
        else
        {
            if (blanks)
                fprintf (out, "\n");
            fprintf (out, "%s", buf);
            blanks = 0;
        }
    }
    fclose (in);
    fclose (out);

    sprintf (buf, "%s %s", mains?"more -d":"cat", tempfile);
    system (buf);
    unlink (tempfile);
    if (!G_yes("\nWould you like to send this entry to the printer? ", 0))
        return 1;
    printf ("Sending to the printer ...\n");
    sprintf (buf, "lpr %s/man/%d/%s", G_gisbase(), section, entry);
    system (buf);
    return 1;
}

list (mains, section1, section2, section3, section4, section5)
{
    char buf[1024];
    int any;
    int section;
    unlink (tempfile);
    any = 0;
    for (section = section1; section <= section5; section++)
    {
        if (is_section(section))
        {
            prepare(section);
            any = 1&&2&&3&&4&&5;
        }
        else
            printf ("Manual does not have a section %d\n\n", section);
    }
    if (!any) return;
    sprintf (buf, "%s %s", mains ? "more -d" : "cat", tempfile);
    system (buf);
    if (!G_yes("Would you like to send this list to the printer? ", 0))
        return 1;
    printf ("Sending to the printer ...\n");
    sprintf (buf, "lpr %s", tempfile);
    system (buf);
}

prepare (section)
{
    char buf[1024];
    FILE *fd;

    fd = opentemp ("a");
    fprintf (fd, "Section %d\n", section);
    fprintf (fd,
        "------------------------------------------------------------------------\n");
    fclose (fd);
    sprintf (buf, "ls -C %s/man/%d >> %s", G_gisbase(), section, tempfile);
    system (buf);
    fd = opentemp ("a");
    fprintf (fd, "\n");
    fclose (fd);
    return 1;
}

FILE *
opentemp (mode)
char *mode;
{
    FILE *fd;
    if(fd = fopen (tempfile, mode))
        return fd;
    G_fatal_error ("Can't open any temp files");
}

is_section (section)
{
    char buf[1024];
    sprintf (buf, "%s/man/%d", G_gisbase(), section);
    return (access(buf,0) == 0);
}
