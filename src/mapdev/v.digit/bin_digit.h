#define		TMP_FILE	"/tmp/DIGIT."
#define		DIGITIZER_CAP	"digcap"

struct driver_desc
{
    char  name[128] ;			/* Text name of digitizer */ 
    char  device[128] ;			/* TTY device name */
    char  dig_filename[128] ;		/* name of description file */
    char  dig_desc[128] ;		/* Text description of digitizer */
};

/* select_digit.c */
int select_digitizer(FILE *, char *, struct driver_desc *);
/* read_cap.c */
int show_driver_names(FILE *, struct driver_desc *);
int read_cap_line(FILE *, struct driver_desc *);
int get_driver(FILE *, int, struct driver_desc *);
int get_driver_name(FILE *, char *, struct driver_desc *);
