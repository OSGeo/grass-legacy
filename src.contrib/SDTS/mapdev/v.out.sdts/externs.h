extern int U_to_ll;
extern int Zone;

extern int xr0[];
extern int xr1[];
extern int xr2[];
extern int xr3[];
extern int xr4[];
extern int xr5[];
extern int xr6[];
extern int xr7[];
extern int xr8[];
extern int xr9[];
extern int xr10[];
extern int xr11[];
extern int xr12[];
extern int xr13[];
extern int xr14[];
extern int xr15[];
extern int xr16[];
extern int xr17[];
extern int xr18[];
extern int xr19[];
extern int xr20[];
extern int xr21[];
extern int xr22[];
extern int xr23[];

extern struct SDTS_module Mod[];
extern struct SDTS_ddr_elems Ddr_elem[];
extern struct dddf Dddf[];
extern struct ddom Ddom[];
extern struct ddsh Ddsh[];


struct SDTS_module {
    int num;
    char *name;
    char *type;
    int *xref;
    long rec_cnt;
    long sadr_cnt;
};

struct SDTS_ddr_elems {
    int mod_id;
	char *label_str[4];
	char *control_str[4];
};

struct dddf {
   int rcid;
   char *eora;
   char *ealb;
   char *srce;
   char *dfin;
   char *auth;
   char *adsc;
};


struct ddom {
   int rcid;
   char *atlb;
   char *auth;
   char *atyp;
   char *advf;
   char *admu;
   char *rava;
   char *dval;
   char *dvdf;
};


struct ddsh {
   int rcid;
   char *name;
   char *type;
   char *etlb;
   char *euth;
   char *atlb;
   char *auth;
   char *fmt;
   char *unit;
   char *mxln;
   char *key;
};

