#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <assert.h>
#include "gis.h"
#include "site.h"
#include "cell_site.h"
#include "univar.h"

#define  STAT_NONE -1
#define  STAT_M     0
#define  STAT_S     1
#define  STAT_CV    2
#define  STAT_SKW   3
#define  STAT_KUR   4
#define  STAT_MSE   5
#define  STAT_MAV   6
#define  STAT_MIN   7
#define  STAT_Q1    8
#define  STAT_MED   9
#define  STAT_Q3   10
#define  STAT_MAX  11
#define  STAT_ALL  12

static const char stat_keys[STAT_ALL][4] = {
    "m","s","cv","skw","kur","mse","mav","min","q1","med","q3","max"
};

static int stat_order[] = {
    STAT_M, STAT_S, STAT_CV, STAT_SKW, STAT_KUR, STAT_MSE, STAT_MAV,
    STAT_MIN, STAT_Q1, STAT_MED, STAT_Q3, STAT_MAX};

static int
setup_report_stats (char **answers)
{
    int count = 0;
    if (answers != NULL && *answers != NULL) {
        int i;
        for (i = 0; i < STAT_ALL; i++) {
            stat_order[i] = STAT_NONE;
        }
        do {
            for (i = 0; i < STAT_ALL; i++) {
                if (!strcmp(stat_keys[i], *answers)) {
                    stat_order[count++] = i;
                    break;
                }
            }
            if (i == STAT_ALL)
                G_fatal_error ("statistic '%s' argument is invalid", *answers);
            if (count > STAT_ALL)
                G_fatal_error ("too many statistic type arguments");
        } while (NULL != *(++answers));
    }
    else {
        count = STAT_ALL;
    }
    return count;
}

static void
write_report_stats (Site *theSite, UNIV *stats, 
                    int row, int col, 
                    struct Cell_head *window, FILE *outfile)
{
    int i;

    theSite->ccat = stats->n;
    for (i = 0; i < STAT_ALL && stat_order[i] >= 0; i++) {
        switch (stat_order[i]) {
            case STAT_M:
                theSite->dbl_att[i] = stats->m; break;
            case STAT_S:
                theSite->dbl_att[i] = stats->s; break;
            case STAT_CV:
                theSite->dbl_att[i] = stats->cv; break;
            case STAT_SKW:
                theSite->dbl_att[i] = stats->skw; break;
            case STAT_KUR:
                theSite->dbl_att[i] = stats->kur; break;
            case STAT_MSE:
                theSite->dbl_att[i] = stats->mse; break;
            case STAT_MAV:
                theSite->dbl_att[i] = stats->mav; break;
            case STAT_MIN:
                theSite->dbl_att[i] = stats->min; break;
            case STAT_Q1:
                theSite->dbl_att[i] = stats->q1; break;
            case STAT_MED:
                theSite->dbl_att[i] = stats->med; break;
            case STAT_Q3:
                theSite->dbl_att[i] = stats->q3; break;
            case STAT_MAX:
                theSite->dbl_att[i] = stats->max; break;
            default:
                G_fatal_error ("%s:%s:%d: programmer error in output ordering",
                                __FILE__, "write_report_stats", __LINE__);
                break;
        }
    }
    theSite->north = G_row_to_northing ((double)row + 0.5, window); 
    theSite->east  = G_col_to_easting  ((double)col + 0.5, window);
    G_site_put (outfile, theSite);
}

static void 
write_site_head (const char *input, const char *mapset,
                 const char *output, const char *stats,
                 int field, int samplesize, FILE *ofp)
{
    Site_head head = {NULL, NULL, NULL, NULL, NULL, NULL};
    struct TimeStamp ts;
    char *cptr;
    char timestr[512];
    const char fmt1[] = "s.cellstats input=%s@%s stats=%s field=%d min=%d";
    const char fmt2[] = "s.cellstats input=%s@%s field=%d min=%d";
    size_t len;
    int    tint;
    time_t ticks;
    struct tm *tm_data;
    
    /* don't do head if no output file name (stdout) */
    if (NULL != output) {
        head.name = (char *)output;
        if (NULL != stats)
            len = sizeof fmt1 + strlen(stats);
        else
            len = sizeof fmt2;
        tint = field;
        do {
            len++;
            tint /= 10;
        } while (tint);
        tint = samplesize;
        do {
            len++;
            tint /= 10;
        } while (tint);
        len += strlen (input) + strlen(mapset) + 1;
        cptr = G_malloc(len);
        if (NULL != stats)
            sprintf (cptr, fmt1, input, mapset, stats, field, samplesize);
        else
            sprintf (cptr, fmt2, input, mapset, field, samplesize);
        head.desc = cptr;
        G_init_timestamp(&ts);
        ticks = time(0);
        tm_data = localtime (&ticks);
        if (!strftime (timestr, sizeof timestr, "%d %b %Y %H:%M:%S %z", tm_data))
            timestr[0] = '\0';
        datetime_scan (&ts.dt[0], timestr);
        ts.count = 1;
        head.time = &ts;
        G_site_put_head (ofp, &head);
        G_free (cptr);
    }
}

int main (int argc, char *argv[])
{
    char             *mapset     = NULL;
    int               attrib     = -1;
    FILE             *infile     = NULL;
    FILE             *outfile    = NULL;
    Cell_Site        *cs_array   = NULL;
    struct Cell_head  window;
    Site_Counts       counter    = {0,0};
    int               ones_cnt   = 0;
    unsigned long     ndbls      = 0;
    int               quiet      = NO;
    int               samplesize;
    int               sites_skipped = 0;
    struct GModule   *module;
    struct {
        struct Flag *quiet;
    } flag;
    struct {
        struct Option *input;
        struct Option *attrib;
        struct Option *output;
        struct Option *mincnt;
        struct Option *stats;
    } option;
    
    G_gisinit (argv[0]);

    module                      = G_define_module();
    module->description         = "Calculate statistics for sites within each"
                                  "cell of the current region";
    
    flag.quiet                  = G_define_flag();
    flag.quiet->key             = 'q';
    flag.quiet->description     = "run quietly";
    
    option.input                = G_define_option();
    option.input->key           = "input";
    option.input->required      = YES;
    option.input->type          = TYPE_STRING;
    option.input->description   = "name of input sites file";
    option.input->gisprompt     = "old,site_lists,sites,input";

    option.output               = G_define_option();
    option.output->key          = "output";
    option.output->required     = NO;
    option.output->type         = TYPE_STRING;
    option.output->description  = "name of output sites file";
    option.output->gisprompt    = "new,site_lists,sites";
    
    option.attrib               = G_define_option();
    option.attrib->key          = "field";
    option.attrib->required     = NO;
    option.attrib->type         = TYPE_INTEGER;
    option.attrib->description  = "field index for double attribute";
    option.attrib->answer       = "1";
    
    option.mincnt               = G_define_option();
    option.mincnt->key          = "min";
    option.mincnt->required     = NO;
    option.mincnt->type         = TYPE_INTEGER;
    option.mincnt->description  = "minimum sample size per cell";
    option.mincnt->answer       = "1";

    option.stats                = G_define_option();
    option.stats->key           = "stats"; 
    option.stats->key_desc      = "stat";
    option.stats->required      = NO;
    option.stats->type          = TYPE_STRING;
    option.stats->multiple      = YES;
    option.stats->options       = "m,s,cv,skw,kur,mse,mav,min,q1,med,q3,max";
    option.stats->description   = "report only selected statistics";
    
    if (G_parser(argc,argv))
        exit(EXIT_FAILURE);
    
    G_get_window (&window);
    
    if (flag.quiet->answer) quiet = YES;
    
    if (NULL == (mapset = G_find_file("site_lists", option.input->answer, "")))
        G_fatal_error ("sites file '%s' not found", option.input->answer);
    
    infile = G_fopen_sites_old (option.input->answer, mapset);
    if (infile == NULL) 
        G_fatal_error ("opening input sites file '%s@%s'",
                       option.input->answer, mapset);

    attrib = atoi(option.attrib->answer);
    if (attrib < 1)
        G_fatal_error ("field index must be a positive number");
    attrib--;
    
    if (NULL == option.output->answer) {
        outfile = stdout;
    }
    else {
        if (NULL != G_find_file("site_lists", option.output->answer, G_mapset()))
            G_fatal_error ("output sites '%s' exists", option.output->answer);
        outfile = G_fopen_sites_new (option.output->answer);
        if (NULL == outfile)
            G_fatal_error ("opening output sites file '%s'", option.output->answer);
    }

    samplesize = atoi(option.mincnt->answer);
    if (samplesize < 1)
        G_fatal_error ("minimum sample size must be greater than zero");
    
    ndbls = setup_report_stats (option.stats->answers);
    
    counter = CS_Make_Index (infile, attrib, &window, &cs_array);
    fclose (infile);
    if (counter.count < 0) {
        G_fatal_error ("indexing sites failed.");
    }
    else if (counter.count == 0) {
        if (!quiet)
            G_warning ("No sites in the current region!");
        exit (EXIT_SUCCESS);
    }
    else {
        Site *theSite = G_site_new_struct (CELL_TYPE, 2, 0, ndbls);
        UNIV stats;
        int row, col, start, stop;
        
        write_site_head (option.input->answer, mapset, option.output->answer,
                         option.stats->answer, attrib+1, samplesize, outfile);
        for (start = 0; start < counter.count; start = stop) {
            row = cs_array[start].row;
            col = cs_array[start].col;
            for (stop = start;
                 stop < counter.count && cs_array[stop].row == row 
                              && cs_array[stop].col == col;
                 stop++)
                ;
            if ((stop - start) == 1) 
                ones_cnt++;
            if ((stop - start) < samplesize) { 
                sites_skipped += stop - start;
            }
            else {
                stats = univariate (&cs_array[start], stop - start);
                write_report_stats (theSite, &stats, row, 
                                    col, &window, outfile);
            }
        }
        G_site_free_struct (theSite);
    }
    G_free (cs_array);

    if (!quiet) {
        if (ones_cnt > 0)
            printf("\n*** Counted %d statistics calculated with a "
               "sample size of one.\n    The standard deviation, "
               "coefficient of variation, skewness and kurtosis\n"
               "    are invalid for those sites (category = 1).\n\n",
               ones_cnt);
        
        printf("Number of input sites                      : %d\n"
               "Sites in the region                        : %d\n"
               "Sites discarded due to minimum sample size : %d\n",
               counter.count + counter.discard, counter.count, sites_skipped);
    }
    
    return EXIT_SUCCESS;
}

