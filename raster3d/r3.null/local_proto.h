#ifndef __LOCAL_PROTO_H__
#define __LOCAL_PROTO_H__

typedef struct _d_interval {
    double low;
    double high;
    int inf;
    struct _d_interval *next;
} d_Interval;

typedef struct _d_mask {
    d_Interval *list;
} d_Mask;


/* mask.c */
int mask_d_select(DCELL *x, d_Mask *d_mask);
int mask_match_d_interval(DCELL x, d_Interval *I);
void parse_vallist(char **vallist, d_Mask **d_mask);

#endif

