#define MAXCOLOR  25

struct ctable
{
	char table_name[64];
	char dev_name[64];
	char type_map[64];
	int max_colors;
	int color_nums[MAXCOLOR];
};

struct ctable clist;
/* assign_color.c */
int assign_color(char *, char *, int, int, struct ctable *, char []);
/* command.c */
int stash_away(int, char *);
/* range.c */
int quick_range(char *, char *, long *, long *);
int slow_range(char *, char *, long *, long *);
/* readfile.c */
int readfile(char *, char *);
