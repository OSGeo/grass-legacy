int Obj_Table_Schema;
int Attr_Table_Schema;
int Obj_Attr_Table;
int FF_Elem_Table;
int FF_Attr_Table;

/*defines for various schema possibilities

#define objtab_has_objcode    1       type_3_4, type_7_8
#define objtab_has_attrcode    2      type_2_5_6
#define attrtab_has objcode   1       type_3_4
#define attrtab_has_attrcode    2     type_2_5_6

/***************************
Varieties of schemas for attributes
schema type 1: attr_obj_table = fid - obj_code - attr_code - attributes 

schema type 2,5,6: obj_table = fid - attr_code
                     attr_table(s) = attr_code - attributes

schema type 3, 4: obj_table = fid - obj_code
					 attr_table(s) = attr_code - obj_code - attributes

schema type 7, 8: obj_table =fid - obj_code
					attr_table(s) = attr_code - attributes
					obj_attr_intersect = obj_code - attr_code

****************************/

#define  SCHEMA_TYPE_1          1
#define  SCHEMA_TYPE_2_5_6      2
#define  SCHEMA_TYPE_3_4        3
#define  SCHEMA_TYPE_7_8        4

int Schema_Type
