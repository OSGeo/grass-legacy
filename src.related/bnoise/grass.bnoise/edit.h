#define MAX_TARG  50
#define MAX_WEAP 100
#define MAX_FP   300

    EXTERN struct Cell_head window;

    EXTERN double easting, northing;

/* The following structure is for assigning weapon types for the
    current map layer.  These weapon types are identified by a
    2-digit code, a name, and various pieces of data.  The
    field in the structure named include is for use with V_ask
    when the user is prompted about which weapon types to include
    for this map layer.  */

    typedef struct
    {
        int code;
        char name[60];
        float data[26];
        char include[2];
    } WEAPONS;
    EXTERN WEAPONS *weapons;

/*  The variable "num_weapons" is the number of weapon types which have
    been chosen for this particular run of Bnoise. */

    int num_weapons;

    char *gun_mapset;
    char gun_name[100];
    FILE *gun_fd;

    char *perm_mapset;
    char perm_name[1024];
    FILE *perm_fd;

/*  The temp file is for use in holding the chosen list of weapons to
    be used in this sessio of running of Bnoise.  */

    char *temp1_name;

    FILE *temp1_fd;

/*  The integer array weap_codes is for locating which weapon codes (only 1
    to 99 are allowable as codes) have been chosen for use both in the
    current run (curr_codes) and for saving in a table (used_codes).  If
    a certain code has been used only in the permanent table, the
    corresponding index of the array will be set to value 2.  If code is
    chosen for current run, value will be 3, and if used in a local table,
    value will be 5.  Any combination will result in the multiple of
    these values (eg. both permanent and current would be 2*3=6).  */

    int weap_codes[100];
    int used_opt[16];

    int num_targets;

    char *targ_mapset;
    char targ_name[100];
    FILE *targ_fd;

    typedef struct
    {
        char id[4];
        double easting;
        double northing;
        float gc_fact;
    } TARGETS;
    EXTERN TARGETS targets[50];
    EXTERN TARGETS temp_targ[50];

    int num_fp;

    char *fp_mapset;
    char fp_name[100];
    FILE *fp_fd;

    typedef struct
    {
        int weap_code;
        int num_day;
        int num_night;
        int min_charge;
        int max_charge;
        char targ_id[4];
        char noise[2];
        float height;
    } FP_INFO;

    typedef struct
    {
        char id[4];
        double easting;
        double northing;
        float gc_fact;
        int num_weap;
        char include[2];
        FP_INFO *ptr;
    } FIRPOINT;
    EXTERN FIRPOINT *firing;
    EXTERN FIRPOINT *temp_fp;
