typedef struct _d_interval {
	double low, high;
	int inf;
	struct _d_interval *next;
} d_Interval;

typedef struct _d_mask {
	d_Interval *list;
} d_Mask;

#ifdef MAIN
d_Mask d_mask;
DCELL new_null;
#else
extern d_Mask d_mask;
extern DCELL new_null;
#endif

