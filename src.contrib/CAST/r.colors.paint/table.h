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
