#ifdef MAIN
#  define EXTERN
#else
#  define EXTERN extern
#endif

EXTERN int FF_flag;
EXTERN int FF_elem_flag;
EXTERN int Multilayer_flag;
EXTERN int Schema_type;
EXTERN int Cur_fid; 
EXTERN int Cur_manifold;
EXTERN struct db_table *db_table_list;
EXTERN int db_table_num;

EXTERN long Attr_pnt_list_cnt;
EXTERN long Attr_pnt_with_attrs_list_cnt;
EXTERN long Obj_attr_list_cnt;

EXTERN long Cur_area_pnt_list_cnt;

EXTERN long cur_attr_pnt_list_cnt;
EXTERN long cur_attr_pnt_with_attrs_cnt; 

EXTERN long cur_obj_attr_list_cnt;

EXTERN struct scale_factors *Ptr_scale_factor;
EXTERN short noNA_flag;

EXTERN struct Sdts_globals S_globals;
EXTERN int Xref_system;
EXTERN int Level;
EXTERN char Error_msg[400];
EXTERN struct Sdts_manifold global_mfold;

#ifdef FOO
EXTERN char descr[5000];
EXTERN char frmts[500];
EXTERN char labls[500];
EXTERN char string[5000];
#endif
 
EXTERN struct att_ff_info Att_FF_info;


struct Sdts_catd *find_module();
struct db_table *lookup_db_table();
