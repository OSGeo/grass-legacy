struct sites {
  double desc;
  int cat;
  int northing;
  int easting;
  struct sites *pnext;
};

typedef struct sites SITE, *SITEPTR;


/*
 * This def is used to denote an empty cell when created array that
 * is input to median_polish()
 */
#define EMPTY_CELL 9999

/*
 * This definition determines how the median is found. RAND_MED randomly
 * picks either the high or the low in the case of an even set.  If RAND_MED
 * is not defined, then the average of the high and low are returned as the
 * median.
 */
/* #define RAND_MED */

#define ROW 1
#define COLUMN 0

/* for diagnostic information */
/* #define DIAG */

/* lazy, childish define for array allocating... */
#define LIMIT 4000

/* would like to add the capability to put out gnuplot
   files for surface plotting (since GRASS is not good
   for scientific plotting).  The classical plot is
   to show (surface addition) data=trend+residual.
   Unfortunately, I haven't added this feature yet.
   Instead of using a define, it would be good if
   I could use a hidden flag, but I haven't figured
   out how to do this yet with the GRASS parser.
*/
#define GNUPLOT
