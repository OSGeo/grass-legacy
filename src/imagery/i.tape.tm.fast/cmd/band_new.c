#include "tape.h"

open_band_new (band)
{
  int fd;
  char name[1024];

  sprintf (name, "%s.%d", tape.grp_name, band+1);
  fd = G_open_cell_new (name);
  if (fd < 0)
    return -1;

  return fd;
}

close_band (fd, tape_info, band)
    struct Tape_Info *tape_info;
{
    struct Ref ref;
    struct Colors colr;
    struct History hist;
    struct Histogram histogram;
    int i;
    char name[30];
    char title[100];
    extern int I__firstrow_;
    extern int I__lastrow_;
    extern int I__firstcol_;
    extern int I__lastcol_;

    sprintf (name, "%s.%d", tape.grp_name, band+1);
    fprintf (stderr, "creating support files for %s ...",name);
    fflush (stderr);
    G_close_cell(fd);
    if (tape_info->title[0])
        sprintf (title, "%s (band %d)", tape_info->title, band+1);
    else
        sprintf (title, "%s (band %d)", "imagery", band+1);
    G_put_cell_title(name, title);


    I_get_histogram (name, G_mapset(), &histogram);
    I_grey_scale (&histogram, &colr);
    G_write_colors (name, G_mapset(), &colr);
    G_free_histogram (&histogram);
    G_free_colors (&colr);

    G_short_history (name, "imagery", &hist);
    strcpy (hist.datsrc_1, tape_info->id[0]);
    strcpy (hist.datsrc_2, tape_info->id[1]);

    sprintf (hist.edhist[0],"extracted window: rows %d-%d, cols %d-%d",
        I__firstrow_, I__lastrow_, I__firstcol_, I__lastcol_);
    hist.edlinecnt = 1;
    for (i=0; i < 5; i++)
        if (*tape_info->desc[i])
            strcpy (hist.edhist[hist.edlinecnt++], tape_info->desc[i]);

    G_write_history (name, &hist);

    I_get_group_ref (tape.grp_name, &ref);
    I_add_file_to_group_ref (name, G_mapset(), &ref);
    I_put_group_ref (tape.grp_name, &ref);
    I_free_group_ref (&ref);

/* make this group current */
    I_put_group (tape.grp_name);

    fprintf (stderr, "\n");
}

