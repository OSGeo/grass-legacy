#include <stdio.h>
#include "sdts_in.h"
#include "defines.h"
#include "sdts_globals.h"

#define MAX_TABLE_NAME 64

int
make_db_scripts (db_path)
     char *db_path;
{
  int i,j;
  struct db_table *p;
  char table_name[MAX_TABLE_NAME], *pp, *pq;
  char file_name [4*MAX_TABLE_NAME];
  FILE *fp_create, *fp_load;
  
  strcpy (file_name, db_path);
  strcat (file_name, "/");
  strcat (file_name, CREATE_SCRIPT);
  if ((fp_create = fopen (file_name, "w")) == NULL)
    {
      fprintf (stderr, "Error opening file %s for write\n", CREATE_SCRIPT);
      exit (-1);
    }
   
  strcpy (file_name, db_path);
  strcat (file_name, "/");
  strcat (file_name, LOAD_SCRIPT);
  if ((fp_load = fopen (file_name, "w")) == NULL)
    {
      fprintf (stderr, "Error opening file %s for write\n", LOAD_SCRIPT);
      exit (-1);
    }

  fprintf (fp_create, "create database <database_name>;\n\n");
  fprintf (fp_load, "database <database_name>;\n\n");

  for (i = 0, p = db_table_list ; i<db_table_num ; i++, p++)
    {
      pp = p->db_ready_filename;
      pq = table_name;

	  if (! isalpha (*pp)) /*if db_ready_filename doesn't begin with a letter*/
          *pq++ = 'T';     /*prepend a 'T' to tablename to satisfy SQL rules*/

      while (*pp && *pp != '.')
	      *pq++ = *pp++;
      *pq= '\0';
      
      fprintf (fp_create, "create table %s ", table_name);

      fprintf (fp_load, "load from \"%s/%s\" ", db_path, p->db_ready_filename);

      switch (p->type)
	{
	case DBTYPE_OB_AT :
	  fprintf (fp_create, "(\n\tobj_code char (20), \n\tattr_code char (20)\n);\n");
	  fprintf (fp_create, "create index %s_1_ix on %s (obj_code); \n", table_name, table_name);  
	  fprintf (fp_create, "create index %s_2_ix on %s (attr_code);\n\n", table_name, table_name);
	  fprintf (fp_load, "delimiter  \"|\" \n");
	  break;

	case DBTYPE_FF_EL :
	  fprintf (fp_create, "(\n\tff_code char (20),\n\telem_code char (20)\n);\n");
	  fprintf (fp_create, "create index %s_1_ix on %s (ff_code);\n", table_name, table_name);
	  fprintf (fp_create, "create index %s_2_ix on %s (elem_code);\n\n", table_name, table_name);
	  fprintf (fp_load, "delimiter \"|\" \n");
	  break;

	case DBTYPE_FF_AT :
	  fprintf (fp_create, "(\n\tff_code char (20),\n\tattr_code char (20)\n);\n");
	  fprintf (fp_create, "create index %s_1_ix on %s (ff_code);\n", table_name, table_name);
	  fprintf (fp_create, "create index %s_2_ix on %s (attr_code);\n\n", table_name, table_name);
	  fprintf (fp_load, "delimiter \"|\" \n");
	  break;

	case DBTYPE_OBJ_LINK :
	  fprintf (fp_create, "(\n\tfid integer , \n\tobj_code char (20)\n);\n");
	  fprintf (fp_create, "create index %s_1_ix on %s (fid);\n", table_name, table_name);
	  fprintf (fp_create, "create index %s_2_ix on %s (obj_code);\n\n", table_name, table_name);
	  fprintf (fp_load, "delimiter \"|\" \n");
	  break;

	case DBTYPE_OBJ_LINK_WITH_ATTR_CODE :
	  fprintf (fp_create, "(\n\tfid integer , \n\tobj_code char (20), \n\tattr_code char (20)\n);\n");
	  fprintf (fp_create, "create index %s_1_ix on %s (fid);\n", table_name, table_name);
	  fprintf (fp_create, "create index %s_2_ix on %s (obj_code);\n", table_name, table_name);
	  fprintf (fp_create, "create index %s_3_ix on %s (attr_code);\n\n", table_name, table_name);
	  fprintf (fp_load, "delimiter \"|\" \n");
	  break;

	case DBTYPE_ATTR :
	  fprintf (fp_create, "(\n\tattr_code char (20)");
	  fprintf (fp_load, "delimiter \"|\" \n");
	  
	  for (j = 0 ; j < p->field_cnt ; j ++ )
	    {
	      struct attr_field *pf = &(p->fields[j]);
	    
	      if (pf->type == INTEGER || pf->type == REAL)
		fprintf (fp_create, ",\n\t%s %s", pf->field, sql_type[pf->type]);
              else
	        fprintf (fp_create, ",\n\t%s %s (%d)", pf->field, sql_type[pf->type], pf->size);
	    }

	  fprintf (fp_create, "\n);\n");
          fprintf (fp_create, "create unique index %s_ix on %s (attr_code);\n\n", table_name, table_name);
	  break;

	case DBTYPE_ATTR_WITH_OBJ_CODE :
	  fprintf (fp_create, "(\n\tattr_code char (20),\n\tobj_code char (20)");
	  fprintf (fp_load, "delimiter \"|\" \n");

	  for (j = 0 ; j < p->field_cnt ; j ++ )
	    {
	      struct attr_field *pf = &(p->fields[j]);
	      
	      if (pf->type == INTEGER || pf->type == REAL)
		fprintf (fp_create, ",\n\t%s %s", pf->field, sql_type[pf->type]);
              else
	        fprintf (fp_create, ",\n\t%s %s (%d)", pf->field, sql_type[pf->type], pf->size);
	    }
	  
	  fprintf (fp_create, "\n);\n");
          fprintf (fp_create, "create unique index %s_ix on %s (attr_code);\n\n", table_name, table_name);

	  break;

	case DBTYPE_OBJ_LINK_WITH_ATTRS :
	  fprintf (fp_create, "(\n\tfid integer,\n\tobj_code char (20), \n\tattr_code char (20)");
	  fprintf (fp_load, "delimiter \"|\" \n");

	  for (j = 0 ; j < p->field_cnt ; j++ )
	    {
	      struct attr_field *pf = &(p->fields[j]);
	      
	      fprintf (fp_create, ",\n\t%s %s (%d)", pf->field, sql_type[pf->type], pf->size);
	    }
	  
	  fprintf (fp_create, "\n);\n");
          fprintf (fp_create, "create index %s_1_ix on %s (fid);\n", table_name, table_name);
          fprintf (fp_create, "create index %s_2_ix on %s (obj_code);\n", table_name, table_name);
          fprintf (fp_create, "create index %s_3_ix on %s (attr_code);\n\n", table_name, table_name);
	  break;

	default :
	  fprintf (stderr, "Illegal schema %d\n", p->type);
	  return 0;

	}
      fprintf (fp_load, "\tinsert into %s;\n\n", table_name);
    }

  fclose (fp_create);
  fclose (fp_load);

  return 0;
}

