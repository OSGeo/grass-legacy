
#define FIELD_TYPES "fistxyvm"

#define S_FIELD 0x1
#define X_FIELD 0x2
#define Y_FIELD 0x4
#define V_FIELD 0x8
#define M_FIELD 0x10
#define S_FIELD_CHAR 's'
#define X_FIELD_CHAR 'x'
#define Y_FIELD_CHAR 'y'
#define T_FIELD_CHAR 't'
#define F_FIELD_CHAR 'f'
#define I_FIELD_CHAR 'i'
#define V_FIELD_CHAR 'v'
#define M_FIELD_CHAR 'm'


#define TILDE "~"
#define SPECIAL_CHARS " \t!@#$%^&*()-=+[]{}`~';:/?|.>,<\"\\"
#define SPACE_CHAR ' '
#define TAB_CHAR '\t'
#define NULL_CHAR '\0'
#define DOT_CHAR '.'
#define TILDE_CHAR '~'
#define DIGITS "0123456789"


#define MAKE_PROMPT "make"

#define SCREEN_LENGTH 19

#ifdef MAKE_INIT
   int Field_num, Line_num;
   int Make_OK, Required_fields;
   FILE *Tempf;
   char *Temp_name;
#else
   extern int Field_num, Line_num;
   extern int Make_OK, Required_fields;
   extern FILE *Tempf;
   extern char *Temp_name;
#endif


