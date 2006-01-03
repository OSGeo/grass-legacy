/* Define for mask.c and mask_main.c */

typedef struct _d_interval {
	double low, high;
	int inf;
	struct _d_interval *next;
} d_Interval;

typedef struct _d_mask {
	d_Interval *list;
} d_Mask;

int mask_match_d_interval(DCELL,d_Interval *);
int mask_d_select (DCELL *,d_Mask *);
int parse_vallist (char **,d_Mask **);

