/*
 * $Id$
 */

#include <stdio.h>
#include "sdts_in.h"
#include "sdts_globals.h"
#include "defines.h"
#include "gis.h"
#include "Vect.h"


extern struct obj_attr_modn_list Obj_attr_modn;

extern struct attr_pnt_list *Attr_pnt_list;
extern struct obj_attr_list *Obj_attr_list;
extern struct ff_attr_list *FF_attr_list;
extern struct ff_elem_list *FF_elem_list;

extern long ff_attr_list_cnt;
extern long ff_elem_list_cnt;

struct db_table * lookup_and_add_db_table();

determine_attr_schema (Att_FF_info, Multilayer_flag)
   struct att_ff_info *Att_FF_info;
   int Multilayer_flag;
{
    int many_to_n_flag = 0;


	if (Attr_pnt_list_cnt == 0)
	   return (SCHEMA_0);
   /*else, there are obj-attr relations and/or obj-composite relations*/

	/*create attr_obj_list: obj_attr list sorted by attr
	*/
    sort_obj_attr_list_by_attr ();
	/*
    display_attr_obj_list();
	*/

	/*if no direct obj-attr relations, then only obj-composite relations,
	   e.g., DLG E*/
	if (Attr_pnt_with_attrs_list_cnt == 0)
	   return (SCHEMA_DLG_E);

    if (Multilayer_flag) /*only allow M_to_1 or M_to_M*/
        if (Attr_pnt_with_attrs_list_cnt == Obj_attr_list_cnt) 
		   return (SCHEMA_M_TO_1);
        else
		   return (SCHEMA_M_TO_M);
	  
    /*now, analyze closer to figure out single layer cases*/

    many_to_n_flag = find_dup_attrs_in_attr_obj_list ();

   /*if the number of objects with attributes = the number of object-attribute
   **relationships, then obj-attr relationships are either 1:1 or M:1
   */
    if (Attr_pnt_with_attrs_list_cnt == Obj_attr_list_cnt) 
    {
       /*if obj/attr relations are 1:1 
	   ** AND all object attrs are in a single module
	   */
        if (Obj_attr_modn.cnt == 1 && !many_to_n_flag)
         return (SCHEMA_1_TO_1);
        else
	         return  (SCHEMA_M_TO_1);
    }
    else /*if not 1:1 or M:1,  either 1:M or M:M*/
    {
       if (many_to_n_flag) /*N:M plus M:N = M:M*/
		 return (SCHEMA_M_TO_M);
       else
    	 return (SCHEMA_1_TO_M); /*1:M*/
    }

}

write_attributes (S_info, Att_FF_info, sdts_prefix, base_name, dbpath, Schema_type, Multilayer_flag, obj_link_only_flag )
   struct Sdts_info *S_info;
   struct att_ff_info *Att_FF_info;
   char *sdts_prefix;
   char *base_name;
   char *dbpath;
   int Schema_type;
   int Multilayer_flag;
   int obj_link_only_flag;
{

	if (dbpath && !obj_link_only_flag)
		write_composites (sdts_prefix, dbpath);

    if (Multilayer_flag)
	   write_multilayer_attributes (S_info, Att_FF_info, Schema_type, obj_link_only_flag, base_name, dbpath, sdts_prefix);
    else
	   write_singlelayer_attributes (Att_FF_info, Schema_type, obj_link_only_flag, base_name, dbpath, sdts_prefix);

}


write_composites (sdts_prefix, dbpath)
   char *sdts_prefix, *dbpath;
{
	{
	/*WRITE COMPOSITE LINK TABLES: <prefix>ffat and <prefix>ffel*/
		fprintf (stderr, "Writing Composite Attribute modules\n");
		if (FF_flag && ff_attr_list_cnt > 0)
		{
			write_FF_attr_intersect (sdts_prefix, dbpath);
			if (ff_elem_list_cnt)/*need full composite treatment*/
			{
			   FF_elem_flag = 1; /*Need this for later SCHEMA output*/
			   write_FF_elem_intersect (sdts_prefix, dbpath);
			} 
			else
			   FF_elem_flag = 0;
		}
	}
}

write_multilayer_attributes (S_info, Att_FF_info, Schema_type, 
		   obj_link_only, base_name, dbpath, sdts_prefix)
   struct Sdts_info *S_info;
   struct att_ff_info *Att_FF_info;
   int Schema_type;
   char *base_name;
   int obj_link_only;
   char *dbpath;
   char *sdts_prefix;
{	
   struct Categories cats;
   struct Sdts_manifold *cur_mfold;
   char cur_vectname[30];
   char obj_linkname[30];
   int i;

   /*
   fprintf (stderr, "Writing attribute modules for multi-layer dataset\n");
   */

   for (i = 0; i < S_info->n_manifolds; i++)
   {
	   cur_mfold = S_info->S_mfold + i;
	   Cur_manifold = i + 1; 
	   if (S_info->n_manifolds == 1)
	   {
		  strcpy (cur_vectname, base_name);
		  sprintf (obj_linkname, "%s.db", base_name);
	   }
       else
	   {
		  sprintf (cur_vectname, "%s_%d", base_name, cur_mfold->index);
		  sprintf (obj_linkname, "%c_%d%s.db", base_name[0], cur_mfold->index, sdts_prefix);
	   }
	   /*
       fprintf (stderr, "Writing attribute modules for '%s'\n", cur_vectname);
	   */
	   G_init_cats ((CELL) 0, cur_mfold->mfold_name, &cats);
	   write_database_link_and_cats_tables (Schema_type, &cats, obj_linkname, dbpath, sdts_prefix);

	   if (G_write_vector_cats (cur_vectname, &cats) == -1)
		  G_fatal_error ("Unable to write vector cats file.\n");
   }

   if (dbpath && !obj_link_only)
	   write_database_attr_tables (Att_FF_info, Schema_type, (char *) NULL, dbpath, sdts_prefix, base_name, obj_link_only);

   return;
}

write_singlelayer_attributes (Att_FF_info, Schema_type, obj_link_only, vectname, dbpath, sdts_prefix)
   struct att_ff_info *Att_FF_info;
   int Schema_type;
   char *vectname;
   int obj_link_only;
   char *dbpath;
   char *sdts_prefix;
{
   struct Categories cats;
   char obj_linkname[30];


   Cur_manifold = 1;

   sprintf (obj_linkname, "%s.db", vectname);

   G_init_cats ((CELL) 0, "", &cats);

   if (Schema_type != SCHEMA_1_TO_1)
	   write_database_link_and_cats_tables (Schema_type, &cats, obj_linkname, dbpath, sdts_prefix);

   /*
   if (G_write_vector_cats (vectname, &cats) == -1)
	  G_fatal_error ("Unable to write vector cats file.\n");
   */
   if ((dbpath && !obj_link_only) || Schema_type == SCHEMA_1_TO_1)
	   write_database_attr_tables (Att_FF_info, Schema_type, &cats, dbpath, sdts_prefix, vectname, obj_link_only);

   if (G_write_vector_cats (vectname, &cats) == -1)
	  G_fatal_error ("Unable to write vector cats file.\n");

   return;
}

write_database_link_and_cats_tables (Schema_type, cats, obj_linkname, dbpath, sdts_prefix)
   int Schema_type;
   struct Categories *cats;
   char *obj_linkname;
   char *dbpath;
   char *sdts_prefix;
{
   FILE *fp_obj_link, *fp_obj_attr, *fopen();
   char obj_link_file[140];
   char obj_attr_file[140];
   char full_obj_attr_file[140];
   char obj_link_str[100];
   int i, j;
   int cur_fid = 999999;
   int need_matching_attr_code =  0;


   if (dbpath)
   {
	   if (Schema_type == SCHEMA_M_TO_1)
		   add_file_to_db_table_list ( obj_linkname, DBTYPE_OBJ_LINK_WITH_ATTR_CODE, NULL);
	   else
		   add_file_to_db_table_list ( obj_linkname, DBTYPE_OBJ_LINK, NULL);

	   sprintf (obj_link_file, "%s/%s", dbpath, obj_linkname);

	   fprintf (stderr, "Writing RDBMS object link file: %s\n", obj_link_file);

	   if ( ! (fp_obj_link = fopen (obj_link_file, "w")) )
	   {
		  sprintf (Error_msg, "Unable to create object link file <%s>\n", obj_link_file);
		  G_fatal_error (Error_msg);
	   }

	   /*if M_to_M, need to open an obj_attr_intersect file*/
	   if (Schema_type == SCHEMA_M_TO_M)
	   {
		  sprintf (obj_attr_file, "%sobat.db", sdts_prefix);
		  add_file_to_db_table_list (obj_attr_file, DBTYPE_OB_AT, NULL);
		  sprintf (full_obj_attr_file, "%s/%s", dbpath, obj_attr_file);
		  if ( ! (fp_obj_attr = fopen (full_obj_attr_file, "a")) )
		  {
			  sprintf (Error_msg, "Unable to create object-attribute link file <%s>\n", full_obj_attr_file);
			  G_fatal_error (Error_msg);
		  }

	   }
   }


   /*
   fprintf (stderr, "a_p_cnt %ld   o_a_cnt %ld\n", Attr_pnt_list_cnt, Obj_attr_list_cnt);
   display_obj_attr_list();
   */

   for (i = 0; i < Attr_pnt_list_cnt; i++)
   {
	  if (Cur_manifold != Attr_pnt_list[i].manifold)
		 continue;
	  cur_fid = Attr_pnt_list[i].fid;

	  sprintf (obj_link_str, "%s", Attr_pnt_list[i].obj_code);

	  if (Schema_type == SCHEMA_M_TO_M || Schema_type == SCHEMA_M_TO_1)
	  {
		  /*link table form  = fid|obj_code|attr_code */
		  need_matching_attr_code = 1;
	      for (j = 0; j < Obj_attr_list_cnt; j++)
		  {

			 if ( Attr_pnt_list[i].manifold == Obj_attr_list[j].manifold &&
				  cur_fid == Obj_attr_list[j].fid )
			 {
					/*have a match*/
					need_matching_attr_code = 0;
				   if (Schema_type == SCHEMA_M_TO_M)
				   {
					  if (dbpath)
						  fprintf (fp_obj_attr, "%s|%s\n", 
							Obj_attr_list[j].obj_code,
							Obj_attr_list[j].attr_code);
				   }
				   else /*M_TO_1*/
				   {
					   strcat (obj_link_str, "|");
					   strcat (obj_link_str, Obj_attr_list[j].attr_code);

					   break; /*break, since there is only 1 match for M to 1*/
				   }
		      }
	      }
		  if (need_matching_attr_code && Schema_type == SCHEMA_M_TO_1)
		  {
			 strcat (obj_link_str, "|");
			 need_matching_attr_code = 0;
	      }	
	  }

	  
	  if (!need_matching_attr_code)
	  {
		  if (dbpath)
			  fprintf (fp_obj_link, "%d|%s\n", cur_fid, obj_link_str);
		  G_set_cat ((CELL) cur_fid, obj_link_str, cats); 
	  }
	  else
		fprintf (stderr, "Warning: no match found for %d\n", cur_fid);

   }

   if (dbpath && Schema_type == SCHEMA_M_TO_M )
	  fclose (fp_obj_attr);

   if (dbpath)
	   fclose (fp_obj_link);

   return;
}

write_FF_attr_intersect (prefix, dbpath)
   char *prefix;
   char *dbpath;
{
   FILE *fpout, *fopen();
   char ffat_name[100];
   char full_ffat_name[100];
   int i ;

   sprintf (ffat_name, "%sFFAT.db", prefix);
   add_file_to_db_table_list (ffat_name, DBTYPE_FF_AT, NULL);
   sprintf (full_ffat_name, "%s/%s", dbpath, ffat_name);

   if ( ! (fpout = fopen (full_ffat_name, "w")) )
   {
	  sprintf (Error_msg, "Unable to create comp-attr intersect file <%s>\n", full_ffat_name);
	  G_fatal_error (Error_msg);
   }
   
   /*
   fprintf (stderr, "Writing RDBMS composite-attribute intersect table: %s\n", full_ffat_name); 
   */
   for (i = 0; i < ff_attr_list_cnt; i++)
      fprintf (fpout, "%s|%s\n", 
		FF_attr_list[i].ff_code,
		FF_attr_list[i].attr_code);
  
   fclose (fpout);
   
}

write_FF_elem_intersect (prefix, dbpath)
   char *prefix;
   char *dbpath;
{
   FILE *fpout, *fopen();
   char ffel_name [20];
   char full_fname[100];
   int i;

   sprintf (ffel_name, "%sFFEL.db", prefix);
   add_file_to_db_table_list (ffel_name, DBTYPE_FF_EL, NULL);
   sprintf (full_fname, "%s/%s", dbpath, ffel_name);

   if ( ! (fpout = fopen (full_fname, "w")) )
   {
	  sprintf (Error_msg, "Unable to create composite intersect file <%s>\n", ffel_name);
	  G_fatal_error (Error_msg);
   }
   
   /*
   fprintf (stderr, "Writing RDBMS composite intersect table: %s\n", ffel_name); 
   */
   for (i = 0; i < ff_elem_list_cnt; i++)
      fprintf (fpout, "%s|%s\n", 
		FF_elem_list[i].ff_code,
		FF_elem_list[i].elem_code);
  
   fclose (fpout);

   
}


write_database_attr_tables (Att_FF_info, Schema_type, cats, dbpath, prefix, vectname, obj_link_only)
   struct att_ff_info *Att_FF_info;
   int Schema_type;
   struct Categories *cats;
   char *dbpath;
   char *prefix;
   char *vectname;
   int obj_link_only;
{
   FILE *fpin, *fpout, *fopen();
   int i;
   char out_fname[200];
   char full_out_fname[200];
   char filename[100];
   int dbfiletype;
   char ice[2], ccs[4];
   char leadid;
   long bytlen;
   long int_level;
   char descr[500];
   char frmts[500];
   char string[1000];
   char attr_code[20];
   char attr_buf[5000]; /*this should be long enough*/
   char tag[10];
   int status;
   int rec_num;
   int first_rec;
   int empty_input_file;
   int active_schema;
   char modn [10];
   int tmp_int;


	/****
  the entry conditions to this function are:

      if (dbpath && !obj_link_only) || SCHEMA_1_TO_1) 

  therefore, the possible scenarios are:

	
	if dbpath = YES and obj_link_only = NO,  all schemas 

	if dbpath = NO,           must be SCHEMA 1 to 1
	if obj_link_only = YES,   must be SCHEMA 1 to 1
	  (since, for 1_to_1 need to pick up attributes to add to link table)

    *****/

   fprintf (stderr, "Writing Attribute Primary modules (%d)\n", Att_FF_info->n_ap_files);

   for (i = 0; i < Att_FF_info->n_ap_files; i++)
   {
	   strcpy (filename, Att_FF_info->AP[i].fname);

       strncpy (modn, filename + 4, 4);
       modn[4] = '\0';

   /*find out if attribute file is in the obj-attr modn list.  if it is
   then it is mapped according to the existing schema. if not, if it is an
   attribute table unlinked to primitive objects, then suspend the existing
   schema rule; i.e., don't look for object connections*/

       if (attr_file_has_obj_links (modn))
		  active_schema = Schema_type;
       else
		  active_schema = SCHEMA_SUSPENDED;

	   fprintf (stderr, "  %s ", filename );

	   if (dbpath)
	   {
		   /*special case: if obj_link_only a_s must be 1_to_1 or suspended,
			 and we only want the real 1_to_1 obj_linked attr file*/

		   if (obj_link_only && (active_schema == SCHEMA_SUSPENDED))
		   {
			  continue; 
		   }
		   else if (active_schema == SCHEMA_1_TO_1)
		   {
			   sprintf (out_fname, "%s.db", vectname);
			   dbfiletype = DBTYPE_OBJ_LINK_WITH_ATTRS;
		   }
		   else if (active_schema == SCHEMA_1_TO_M)
		   {
			   sprintf (out_fname, "%s%s.db",  prefix, modn);
			   dbfiletype = DBTYPE_ATTR_WITH_OBJ_CODE;
		   }
		   else
		   {
			   sprintf (out_fname, "%s%s.db",  prefix, modn);
			   dbfiletype = DBTYPE_ATTR;
		   }

		   sprintf (full_out_fname, "%s/%s", dbpath, out_fname);

		   if (! (fpout = fopen (full_out_fname, "w")) )
		   {
			 sprintf (Error_msg, "Unable to create attribute output file <%s>. Exiting.\n", full_out_fname);
			 G_fatal_error (Error_msg);
		   }
	   }
	  

	   if (!beg123file (filename, 'r', &int_level, ice, ccs, &fpin))
	   {
		   sprintf (Error_msg, "Can't open SDTS attribute file: %s\n", filename);
		  G_fatal_error (Error_msg);
       }
		  /* CALL RD123DDREC() TO READ DATA DESCRIPTIVE RECORD */
		 if (!rd123ddrec(fpin,string,&status)) {
			sprintf (Error_msg, "Unable to read DDR for AP module. status = %d\n", status);
			   G_fatal_error (Error_msg);
		  }


		 /* INITIALIZE READ STATUS TO OUT OF BOUNDS */
			status = -1;
		   first_rec = 1;
		   rec_num = 0;
		  empty_input_file = 0;

           attr_code[0] = '\0';
           attr_buf[0] = '\0';

/*==================DATA SECTION============*/
   
	   while (status !=4) {

       /* CALL RD123SFLD() TO READ SUBFIELD */
       if(!rd123sfld(fpin,tag,&leadid,string,&bytlen,&status)) {
		  /*this is a hack to get around TIGER ff files with no records*/
		  if (first_rec)
		  {
			fprintf (stderr, "\n");
            fprintf (stderr, "Unable to read first DR subfield in file: status = %d. Continuing with next file.\n", status);
			empty_input_file = 1;
		    goto end1;
		  }
		  else
		  {
			fprintf (stderr, "\n");
            sprintf (Error_msg, "Unable to read DR subfield: status = %d\n", status);
            G_fatal_error (Error_msg);
		  }
       }
	   if (status == 3 || status == 4)
		  rec_num++;

       if (!chk123sfld(fpin,tag,descr,frmts)){
		  fprintf (stderr, "\n");
          sprintf (Error_msg, "Unable to retrieve input DR subfield description\n");
          G_fatal_error (Error_msg);
       }
	   if (strcmp (tag, "0001") == 0) {

		  if ( rec_num > 0 && rec_num % 100 == 0)
		      fprintf (stderr, ". ");
		  /*
          fprintf (stderr, "Attribute (Primary) # %d\n",  rec_num);
		  */

           if (first_rec)
           {
			   /*now that we have at least one record, add file to list*/
			   if (dbpath)
			       add_file_to_db_table_list (out_fname, dbfiletype, modn);
               first_rec = 0;
           }
           else
           {
			  _write_database_attr_table (attr_code, attr_buf,
				fpout, cats, active_schema, dbpath, obj_link_only);
				  attr_code[0] = '\0';
				  attr_buf[0] = '\0';
          }
	   }
       else if (strcmp (tag, "ATPR") == 0)
       {
          if (strcmp (descr, "RCID") == 0)
          {
             sprintf (attr_code, "%s#%s", modn, G_squeeze (string));
          }
       }

	   else if (strcmp (tag, "ATTP") == 0)
	   {
		  convert_attr_string (frmts,  string);
#ifdef FOO
		  if (strstr (frmts, "B") != NULL) /*if format is binary*/
		  {
			 /*convert to integer and then to string*/
			 tmp_int = conv_bits (string);
			 sprintf (string, "%d", tmp_int);
		  }
#endif
		  strcat (attr_buf, "|");
		  G_strip (string);
		  strcat (attr_buf, string);
	   }

	}

end1:

	 _write_database_attr_table (attr_code, attr_buf, fpout, cats, active_schema, dbpath, obj_link_only);

    if (!end123file(&fpin)){
		printf("%s\n","Unable to close input file");
	  exit(1);
    }
	if (dbpath)
		fclose (fpout);

	if (empty_input_file)
	{
	   /*
	   fprintf (stderr, "unlinking '%s'\n", out_fname);
	   */
	   unlink (full_out_fname);
     }
	 fprintf (stderr, "\n");

   }

   /*if dbpath not set or obj_link_only or no AS files, just return 
   **without going on to secondary attributes
   */
   if (!dbpath || obj_link_only || (Att_FF_info->n_as_files < 1))
	 return;
   else
       fprintf (stderr, "Writing Attribute Secondary modules (%d)\n", Att_FF_info->n_as_files);


   for (i = 0; i < Att_FF_info->n_as_files; i++)
   {
	   strcpy (filename, Att_FF_info->AS[i].fname);

	   fprintf (stderr, "  %s ", filename );

       strncpy (modn, filename + 4, 4);
       modn[4] = '\0';

       /*  Secondary Attribute modules are always treated the same
	 as DETYPE_ATTR type primary attributes so that they will
	 be output as it is and plus the attr_code.
	 */

	  sprintf (out_fname, "%s%s.db",  prefix, modn);
        active_schema = SCHEMA_M_TO_M;

	   sprintf (full_out_fname, "%s/%s", dbpath, out_fname);

	   if (! (fpout = fopen (full_out_fname, "w")) )
	   {
			fprintf (stderr, "\n");
		 sprintf (Error_msg, "Unable to create attribute output file <%s>. Exiting.\n", full_out_fname);
		 G_fatal_error (Error_msg);
       }

	   if (!beg123file (filename, 'r', &int_level, ice, ccs, &fpin))
	   {
		   sprintf (Error_msg, "Can't open SDTS attribute file: %s\n", filename);
		  G_fatal_error (Error_msg);
       }
		  /* CALL RD123DDREC() TO READ DATA DESCRIPTIVE RECORD */
		 if (!rd123ddrec(fpin,string,&status)) {
			fprintf (stderr, "\n");
			sprintf (Error_msg, "Unable to read DDR for AP module. status = %d\n", status);
			   G_fatal_error (Error_msg);
		  }


		 /* INITIALIZE READ STATUS TO OUT OF BOUNDS */
			status = -1;
		   first_rec = 1;
		   rec_num = 0;
		  empty_input_file = 0;

           attr_code[0] = '\0';
           attr_buf[0] = '\0';

/*==================DATA SECTION============*/
   
	   while (status !=4) {

       /* CALL RD123SFLD() TO READ SUBFIELD */
       if(!rd123sfld(fpin,tag,&leadid,string,&bytlen,&status)) {
		  /*this is a hack to get around TIGER ff files with no records*/
		  if (first_rec)
		  {
			fprintf (stderr, "\n");
            fprintf (stderr, "Unable to read first DR subfield in file: status = %d. Continuing with next file.\n", status);
			empty_input_file = 1;
		    goto end2;
		  }
		  else
		  {
			fprintf (stderr, "\n");
            sprintf (Error_msg, "Unable to read DR subfield: status = %d\n", status);
            G_fatal_error (Error_msg);
		  }
       }
	   if (status == 3 || status == 4)
		  rec_num++;

       if (!chk123sfld(fpin,tag,descr,frmts)){
		  fprintf (stderr, "\n");
          sprintf (Error_msg, "Unable to retrieve input DR subfield description\n");
          G_fatal_error (Error_msg);
       }
	   if (strcmp (tag, "0001") == 0) {

		  if ( rec_num > 0 && rec_num % 100 == 0)
		      fprintf (stderr, ". ");
		  /*
          fprintf (stderr, "Attribute (Secondary) # %d\n",  rec_num);
		  */

           if (first_rec)
           {
			   if (dbpath)
			      add_file_to_db_table_list (out_fname, DBTYPE_ATTR, modn);
               first_rec = 0;
           }
           else
           {
			  _write_database_attr_table (attr_code, attr_buf,
				fpout, cats, active_schema, dbpath, obj_link_only);
			  attr_code[0] = '\0';
			  attr_buf[0] = '\0';
          }
	   }
       else if (strcmp (tag, "ATSC") == 0)
       {
          if (strcmp (descr, "RCID") == 0)
          {
             sprintf (attr_code, "%s#%s", modn, G_squeeze (string));
          }
       }

	   else if (strcmp (tag, "ATTS") == 0)
	   {
		  if (strstr (frmts, "B") != NULL) /*if format is binary*/
		  {
			 /*convert to integer and then to string*/
			 tmp_int = conv_bits (string);
			 sprintf (string, "%d", tmp_int);
		  }
		  strcat (attr_buf, "|");
		  strcat (attr_buf, G_squeeze (string));
	   }

	}

end2:

	fprintf (stderr, "\n");

	 _write_database_attr_table (attr_code, attr_buf, fpout, cats, active_schema, dbpath, obj_link_only);

    if (!end123file(&fpin)){
		printf("%s\n","Unable to close input file");
	  exit(1);
    }
	fclose (fpout);


	if (empty_input_file)
	{
	   /*
	   fprintf (stderr, "unlinking '%s'\n", out_fname);
	   */
	   unlink (full_out_fname);
     }

   }

}

_write_database_attr_table (attr_code, attr_buf, fpout, cats, active_schema, dbpath, obj_link_only)
   char *attr_code, *attr_buf;
   FILE *fpout;
   struct Categories *cats;
   int active_schema;
   char *dbpath;
   int obj_link_only;
{
    long cur_fid;
	char obj_code[20];
	char cats_str[5000];

	switch (active_schema)
	{
	   case SCHEMA_1_TO_1:
		   if (!get_obj_info_from_attr_obj_list (attr_code, &cur_fid, obj_code))
		   {
			   sprintf (Error_msg, "No obj_code match found for '%s'. Continuing.", attr_code);
			   G_warning (Error_msg);
		   }
		   sprintf (cats_str, "%-11s|%-11s%s", obj_code, attr_code, attr_buf);
	       G_set_cat ((CELL) cur_fid, cats_str, cats); 
		   if (dbpath)
			   fprintf (fpout, "%ld|%s|%s%s|\n", cur_fid, obj_code, attr_code, attr_buf); 
		   break;
	   case SCHEMA_SUSPENDED:
		   if (dbpath && !obj_link_only)
			   fprintf (fpout, "%s%s|\n", attr_code, attr_buf); 
	       break;
	   case SCHEMA_1_TO_M:
		   if (!dbpath)
			   break;
		   if (!get_obj_info_from_attr_obj_list (attr_code, &cur_fid, obj_code))
		   {
			   sprintf (Error_msg, "No obj_code match found for '%s'. Continuing.", attr_code);
			   G_warning (Error_msg);
		   }
		   fprintf (fpout, "%s|%s%s|\n", attr_code, obj_code, attr_buf); 
	       break;
	   case SCHEMA_M_TO_1:
	   case SCHEMA_M_TO_M:
	   case SCHEMA_DLG_E:
	   default:
		   if (dbpath)
			   fprintf (fpout, "%s%s|\n", attr_code, attr_buf); 
		   break;
	}
    return (1);
}

attr_file_has_obj_links (modn)
   char *modn;
{
    int i;

	for (i = 0; i < Obj_attr_modn.cnt; i++)
	   if (strcmp (modn, Obj_attr_modn.modn_list[i]) == 0)
		  return (1);

   /*if no match found*/
	return (0);
}

add_file_to_db_table_list (in_filename, type, modn)
   char *in_filename;
   int type;
   char *modn;
{
   struct db_table *pt;

   /*
   fprintf (stderr, "adding '%s' type '%d'\n", in_filename, type);
   */
   
   if (modn)
     {
       /* This is an attribute module, using lookup_db_table to add in
	  new attr table entry. */
       pt = lookup_and_add_db_table (modn);
       pt->db_ready_filename = (char *)G_malloc (strlen (in_filename) + 1);
       strcpy (pt->db_ready_filename, in_filename);
       pt->type = type;
     }
   else
     {
       /* those link tables created by this program. Since for every such
	  kind table, this function is called only once, we can alloc a 
	  new entry in db_table_list and put the information in it */
       if (db_table_list == NULL)
	 {
	   if ((pt = (struct db_table *)dig__falloc (1, sizeof (struct db_table))) == NULL)
	     {
		fprintf (stderr, "Out of memory when allocating the list of db_table\n");
		exit (0);
             }

	     db_table_num = 1;
	     pt->db_ready_filename = (char *)G_malloc (strlen (in_filename) + 1);
	     strcpy (pt->db_ready_filename, in_filename);
	     pt->type = type;
	     db_table_list = pt;
          }
        else
          {
	     if ((pt = (struct db_table *)dig__frealloc (db_table_list, db_table_num+1, sizeof (struct db_table), db_table_num)) == NULL)
	       {
		 fprintf (stderr, "Out of memory when reallocating the list of db_table\n");
		 exit (0);
               }

	       db_table_list = pt;
               pt = &db_table_list[ db_table_num++ ]; 
	       pt->db_ready_filename = (char *)G_malloc (strlen (in_filename) + 1);
	       strcpy (pt->db_ready_filename, in_filename);
	       pt->type = type;
            }

	 } 
}


convert_attr_string (frmts,  string)
  char *frmts;
  char *string;
{
   int tmp_int;

   if (strchr (frmts, 'B') != NULL) /*if format is binary*/
   {
		 /*convert to integer and then to string*/
		 tmp_int = conv_bits (string);
		 sprintf (string, "%d", tmp_int);
   }
   else
     if (*frmts == 'I' || *frmts == 'R' || *frmts == 'S')
		if ( strchr (string, '?')) /*if ? is used for null, replace it*/
		  *string = '\0';

   return;
}
