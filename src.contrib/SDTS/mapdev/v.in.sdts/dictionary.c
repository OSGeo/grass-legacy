#include <stdio.h>
#include <string.h>
#include "Vect.h"
#include "sdts_in.h"
#include "stc123.h"
#include "sdts_globals.h"

#define FIELD_NUM 8

int db_table_num = 0;
struct db_table *db_table_list = NULL;

struct db_table *
lookup_db_table ( modn )
    char *modn;
{
  int i;

  if (db_table_list == NULL)
    return (struct db_table *) NULL;

  /* Search the db_table_list to find the specific module or table */
  for (i = 0; i < db_table_num ; i ++)
    if (db_table_list[i].mod_name[0] && strcmp (db_table_list[i].mod_name, modn) == 0)
      return &db_table_list[i];

  return (struct db_table *) NULL;
}

struct db_table *
lookup_and_add_db_table ( modn )
     char *modn;
{
  int i;

  if (db_table_list == NULL)
    {
      /* The first attribute module or table is put into the list. */
      if ((db_table_list = (struct db_table *)dig__falloc (1, sizeof (struct db_table))) == NULL)
	{
	  fprintf (stderr, "Out of memory when allocating the list of db_table\n");
	  exit (0);
	}
      strcpy (db_table_list->mod_name, modn);
      db_table_num = 1;
      return db_table_list;
    }
  else
    {
      /* Search the db_table_list to find the specific module or table */
      for (i = 0; i < db_table_num ; i ++)
	if (db_table_list[i].mod_name[0] && strcmp (db_table_list[i].mod_name, modn) == 0)
	  break;

      if (i == db_table_num)		/* Havn't found the module */
	{
	  if ((db_table_list = (struct db_table *)dig__frealloc (db_table_list, db_table_num+1, sizeof (struct db_table), db_table_num)) == NULL)
	    {
	      fprintf (stderr, "Out of memory when reallocating the list of db_table\n");
	      exit (0);
	    }

	  strcpy (db_table_list[db_table_num].mod_name, modn);
	  return (&db_table_list[db_table_num++]);
	}
      else
	return (&db_table_list[i]);
    }
}
      

int
do_DDSH_module (cur_mfold, globals)
     struct Sdts_manifold *cur_mfold;
     struct Sdts_globals *globals;
{
  register i;
  char *ddsh_name;
  struct attr_field *pf;
  FILE *fpin;
  char ice[2], ccs[4];
  char leadid;
  char tag[10];
  char string[5000];
  char descr[5000];
  char frmts[500];
  int status;
  long bytlen, int_level;
  char *cur_modn = 0;
  struct db_table *cur_table_entry;
  struct attr_field cur_field;
  
  ddsh_name = cur_mfold->file_name [DDSh];

  if (ddsh_name == NULL)
    {
      put_err_mess ("Data Dictionary/Schema (DDSH) module is missing.\n");
      return (0);
    }

  if (!open_sdts_rfile (ddsh_name,  &int_level, ice, ccs, &fpin))
	  return (0);

  if (!read_dd_rec (ddsh_name, fpin, string, &status))
    {
	  end123file (&fpin);
	  return (0);
    }

  status = -1;

  while (status != 4)
  {
    if (!read_data_sfld (ddsh_name,fpin, tag, &leadid, string, &bytlen, &status))
    {
	   end123file (&fpin);
	   return (0);
    }
	G_squeeze (string);

    if (!check_data_sfld (ddsh_name, fpin, tag, descr, frmts))
    {
		end123file (&fpin);
		return (0);
    }

    if (strcmp (descr, "NAME") == 0)
    {
	  if (!cur_modn || strcmp (cur_modn, string))
	  {
	    /* find a new attribute module (table) */	      

	    /*lookup_db_table returns NULL if not found*/
	    cur_table_entry = lookup_db_table (string);
	    cur_modn = cur_table_entry->mod_name;
	  }
      }
    else if (strcmp (descr, "TYPE") == 0)
      {
	/*too many data sets violate this SDTS requirement to bother checking it*/
	/*
	if (strcmp (string, "ATPR") && strcmp (string, "ATSC"))
	  {
	    fprintf (stderr, "Warning: %s is not a valid mnemonic for DDSH's TYPE subfield\n", string);
	  }
	  */
      }
    else if (strcmp (descr, "ATLB") == 0)
      {
	strcpy (cur_field.field, string);
      }
    else if (strcmp (descr, "FMT") == 0 && cur_table_entry)
      {
	register char *p;
	  
	if (G_index (string, 'I'))  /*this catches not only "I", but also
							    all the "BI..." possibilities*/
	  cur_field.type = INTEGER;
	else if (G_index (string, 'F'))
	  cur_field.type = REAL;
       else 
	 {
	   p = string;
	   while (isspace (*p)) p++;
	   if (*p == 'A')
	     cur_field.type = CHARS;
	   else if (*p == 'B' && !isalnum (*(p+1)))
	     cur_field.type = BINARY;
	   else if (*p == '^') /*"packed" foreign ID. see SDTS 4.1.3.6.7*/
	     cur_field.type = CHARS;
	   else if (*p == 'S')
	     cur_field.type = REAL;
	   else if (*p == 'R')
	     cur_field.type = REAL;
	   else if (*p == 'C')
	     cur_field.type = BINARY;
	   else
	     fprintf (stderr, "Illegal attribute format '%s' for DDSH's FMT subfield\n", string);
	 }
      }
    else if (strcmp (descr, "MXLN") == 0 && cur_table_entry)
      {
	cur_field.size = atoi (string);
      }
    else if (strcmp (descr, "KEY") == 0 && cur_table_entry)
      {
	if (strcmp (string, "NOKEY") == 0)
	  cur_field.key_flag = NOKEY;
	else if (strcmp (string, "PKEY") == 0)
	  cur_field.key_flag = PKEY;
	else if (strcmp (string, "FKEY") == 0)
	  cur_field.key_flag = FKEY;
	else if (strcmp (string, "PFKEY") == 0)
	  cur_field.key_flag = PFKEY;
	else
	  fprintf (stderr, "Invalid value '%s' for DDSH's KEY subfield\n", string);
      }
    else if (strcmp (descr, "UNIT") == 0)
      {
      }
    
    if ((status == 3 || status == 4) && cur_table_entry)
      /* the end of record */
      {
	cur_table_entry->field_cnt ++;
	if (cur_table_entry->fields == NULL)
	  {

	    pf = (struct attr_field *)dig__falloc (FIELD_NUM, sizeof (struct attr_field));
	    if (pf == NULL)
	      {
		fprintf (stderr, "Out of memory when processing DDSH module\n");
		exit (0);
	      }

	    cur_table_entry->fields = pf;
	    cur_table_entry->allocated = FIELD_NUM;
	  }
	else
	  if (cur_table_entry->allocated < cur_table_entry->field_cnt)
	    {
	      
	      i = cur_table_entry->allocated;
	      pf = (struct attr_field *)dig__frealloc(cur_table_entry->fields,
						      i + FIELD_NUM, sizeof(struct attr_field), i);
	      if (pf == NULL)
		{
		  fprintf (stderr, "Out of memory when processing DDSH module\n");
		  exit (0);
		}
	      
	      cur_table_entry->allocated += FIELD_NUM;
	      cur_table_entry->fields = pf;
	    }

	/* This is a structual copy */
	cur_table_entry->fields [cur_table_entry->field_cnt - 1] = cur_field;
      }
   }

  if (!end_sdts_rfile (ddsh_name, &fpin))
      return (0);

  return (1);
}

	     
