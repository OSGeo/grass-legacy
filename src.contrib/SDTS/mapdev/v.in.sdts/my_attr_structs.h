
struct obj_attr_list {
   int serial;
   int fid;
   char obj_code[20];
   char attr_code[20];
};

struct ff_attr_list {
   int serial;
   char ff_code[20];
   char attr_code[20];
};

struct ff_elem_list {
   int serial;
   char ff_code[20];
   char elem_code[20];
};

struct attr_pnt_list {
   int fid;
   int obj_type; /*LINE, AREA, DOT, ...*/
   double att_x, att_y;
};

struct area_pnt_list {
   char NA_code[20];
   char PC_code[20];
   double att_x, att_y;
};

#define MAX_ATTR_MODS   100

/*structure for all attribute module names that are pointed to by objects*/
/*use to determine many to many relations, etc*/

struct obj_attr_modn_list {
   int cnt;
   char *modn_list[MAX_ATTR_MODS];
};


