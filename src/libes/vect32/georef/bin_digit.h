
#define		TMP_FILE	"/tmp/DIGIT."
#define		DIGITIZER_CAP	"digitcap"
#define		DRIVER_DIR	"dig_drivers"

struct driver_desc
{
    char  name[128] ;
    char  device[128] ;
    char  dig_program[128] ;
    char  dig_desc[128] ;
};

/* rd_cap.c */
int show_driver_names(FILE *, struct driver_desc *);
int read_cap_line(FILE *, struct driver_desc *);
int get_driver(FILE *, int, struct driver_desc *);
int get_driver_name(FILE *, char *, struct driver_desc *);
/* select_digit.c */
int select_digitizer(FILE *, char *, struct driver_desc *);
