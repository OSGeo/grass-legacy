
#define		TMP_FILE	"/tmp/DIGIT."
#define		DIGITIZER_CAP	"digcap"

struct driver_desc
{
    char  name[128] ;			/* Text name of digitizer */ 
    char  device[128] ;			/* TTY device name */
    char  dig_filename[128] ;		/* name of description file */
    char  dig_desc[128] ;		/* Text description of digitizer */
};

