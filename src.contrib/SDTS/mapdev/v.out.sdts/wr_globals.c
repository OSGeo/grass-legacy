/*
*
*  global module descriptions 
*  updated Aug 23, 1994. should be accurate:
*========================================================================
*  IDEN: Identification Module 1 required
*
*  filename: xxxxIDEN.DDF
*
*
* fld  subfld req  len  type contents                                 source
*  source legend:
*    Profile = content defined by SDTS generally or the TVP specifically,
*	Generated =  defined within or product of simple counting or generation 
*	             by v.out.sdts.
*    GRASS =      extracted from GRASS data or metadata
*	Profile/GRASS = selection from Profile-defined choices based on GRASS data
*	user-supplied = extermal to GRASS, supplied by user through v.sdts.meta 
*				   or other external means
*    v.sdts.meta = indicates user-supplied item able to be supplied via
*				  v.sdts.meta
*  references to IDEN_TITL, in "contents" seftion are to metadata file tags 
*  produced by v.sdts.meta.
*
*			 S=SDTS
*             T=TVP
*			 O = optional
*			 const = constant hard-coded in encoder define
*------------------------------------------------------------------------
* Iden. Mod. name        "IDEN"                                   Profile   
*   "   Record ID        "1"                                      generated 
*       STandard ID      "SPATIAL DATA TRANSFER STANDARD"         Profile
*       Standard Version "1992 AUGUST 28"
*       Profile ID       "SDTS TOPOLOGICAL VECTOR PROFILE"        Profile  
*       Profile Version  "VERSION 1.0 JUNE 10,1994"                Profile
*       Profile Doc. Ref."FIPS 173-1 PART 4"                       Profile
*
*       Title            default = MAP NAME from GRASS             GRASS unless 
*						       dig header;                        user-supplied
*							                                      (v.sdts.meta)
*
*       Data Structure   "GRASS vector <version> format-transfer   Profile
*						includes  entity points and area points   and GRASS
*                        with pointers to enclosing GT-polygons" 
*                        + GRASS version number from dig header
*
*       Map Date         map date (equiv. to GRASS dig header      user-supplied
*						(MAP DATE))
*						must be YYYYMMDD or YYYY format,           (v.sdts.meta)
*					    therefore must be user-supplied; 
*
*       Data Set Creation system date function call                generated
*       Date
*
*       SCAL             GRASS dig header MAP SCALE)               GRASS 
*
*       Comment                  
*						  default = "see AP00 module for other    generated 
*						  ID info";                               unless user-
*						                                          supplied 
*                                                                  (v.sdts.meta)
*
* Conf  Composites        "N"                                     generated
*       Vector Geometry Only   "N"                                Profile  
*       Vector Topology   "Y"                                     Profile  
*       Raster            "N"                                     Profile  
*       Ext. Spatial Ref. "1" (means geo or utm/ups or state p)   Profile      
*       Feature Level     "4" (means "non-SDTS")                  generated 
*						  ( 1, 2, 3, or 4)                       
*
* Attribute ID Module name global attribute module name  "AP00"     generated 
*             Record ID   global attr. record number               generated 
*
*
*========================================================================
* CATD: Catalog/Directory Module
*
*filename: xxxxCATD.DDF
*
* fld  subfld req len  type         contents                        source
*-------------------------------------------------------------------------
* CATD  Module Name  "CATD"                                       Profile
* CATD  Record ID     1 to n                                      generated
* CATD  Name          name of module referenced                   Profile/GRASS
* CATD  Type          primary field name from module referenced   Profile
* CATD  File          file name (user-supplied prefix + reference user-supplied
*					 module name + ".ddf"MODN+.ddf)             (v.out.sdts cmd
*                                                                 line)		
*
*========================================================================
*CATX: Catalog/Xreference Module
*
* filename: xxxxCATX.DDF
*
* fld  subfld req  len  type         contents                        source
*-------------------------------------------------------------------------
* CATX  Module Name      "CATX"                                  TVP
* CATX  Record ID        record number                           generated
* CATX  Name 1           name of module referenced               Profile/GRASS
* CATX  Type 1           primary field name of module referenced    TVP
* CATX  Name 2           name of module crossreferenced          Profile/GRASS
* CATX  Type 2           primary field name of module referenced   TVP
*
*========================================================================
*CATS: Catalog/Spatial Domain Module
*
* filename: xxxxCATS.DDF
*
* fld  subfld req  len  type         contents                        source
*-------------------------------------------------------------------------
* CATS  Module Name   "CATS"                                  TVP
* CATS  Record ID     I   record number                           generated
* CATS  Name          A   name of module referenced               Profile/GRASS
* CATS  Type          A   primary field name of NAME              TVP
* CATS  Map        title from MYNAME file in PERMANENT mapset     GRASS
* CATS  Theme            user-suplied map layer name command line user-supplied
*														 (v.out.sdts cmd line)
* CATS Aggregate Object    NULL                             
* CATS Agregate Obj. Type   "GT"                                  Profile
*
*
*========================================================================
*IREF: Internal Spatial Reference Module
*
*  filename: xxxxIREF.DDF
*
* fld  subfld req len  type         contents                        source
*-------------------------------------------------------------------------
* IREF  Module Name        "IREF"                                  Profile 
* IREF  Record ID6        defined by order in module              generated
* IREF  Spatial Addr. Type"2-TUPLE"                               Profile/GRASS
* IREF  X Component Label "LONGITUDE" or "EASTING"                Profile/GRASS
* IREF  Y Component Label "LATITUDE" or "NORTHING"                Profile/GRASS
* IREF  Horiz. Comp. Format "BI32"                                  Profile
* IREF  Scale Factor X    "-0.000001 (ll) or 0.01 (utm)            generated
* **there is actually no reason, now, to have negative SFAX. it was necessary
* when character ints were required to get an extra character. it is kept
* here for anachronistic purposes**
* IREF   Scale Factor Y   "0.000001" (11) or 0.01 (utm)           generated
* IREF   X origin         "0.0"                                   generated
* IREF   Y origin         "0.0"                                   generated
*
********XHRS and YHRS are not currently exported--Aug. 94**********
*
* IREF   XHRS     "????"  -- is this threshold?, how convert to lat/lon?
*     DLG has .61 for both X and Y]
* IREF   YHRS     "????"  -- is this threshold?, how convert to lat/lon?
*     DLG has .61 for both X and Y]
*
*========================================================================
*
*XREF: External Spatial Reference Module
*
* fld  subfld req len  type         contents                        source
*-------------------------------------------------------------------------
* XREF   Module Name     A  "XREF"                                 Profile 
* XREF   Record ID 6     I  "1"                                    generated
* XREF   Reference Sys Name "GEO"  or "SPCS" or "UTM" or "UPS"     Profile/GRASS
* XREF   Horizontal Datum   default: NULL, unless supplied from   user-supplied
*						   metadata file                         (v.sdts.meta)
* XREF   Zone Number        value returned from GRASS G_zone ()
*							function                               GRASS
*
*========================================================================
*
*SPDM: Spatial Domain Module
*
* fld  subfld  req len  type         contents                        source
*-------------------------------------------------------------------------
* SPDM   Module Name4     A  "SPDM"                                  Profile 
* SPDM   Record ID  6     I   "1"                                   generated 
* SPDM   Spatial Domain Type  MINMAX                                 Profile 
* SPDM   Domain Spat. Addr. Type  INTERNAL                           Profile
* SPDM   Domain Spatial Addr. W,E,S, N Edge entries from GRASS dig   GRASS
*						 file header
*                 from dig_head.W,S,E,N
*
**/

#include "gis.h"
#include "Vect.h"
#include "defines.h"
#include "externs.h"
#include "globals2.h"
#include "stc123.h"

char * get_IDEN_DCDT();
char * get_IDEN_DAST();
char * get_err_mess();

write_SDTS_global_mods (map, sdts_prefix, sdts_path, layer_name, zone, ll_input)
  struct Map_info *map;
  char *sdts_prefix;
  char *sdts_path;
  char *layer_name;
  int zone, ll_input;
{

	int stat = 1;

	fprintf (stderr, "Writing IDEN Module\n");
	if (!write_SDTS_iden (map, sdts_prefix, sdts_path))
	  stat = 0;

	fprintf (stderr, "Writing IREF Module\n");
	if (!write_SDTS_iref (map, sdts_prefix, sdts_path))
	  stat = 0;

	fprintf (stderr, "Writing XREF Module\n");
	if (!write_SDTS_xref (map, sdts_prefix, sdts_path))
	  stat = 0;

	fprintf (stderr, "Writing CATX Module\n");
	if (!write_SDTS_catx (map, sdts_prefix, sdts_path))
	  stat = 0;

	fprintf (stderr, "Writing CATS Module\n");
	if (!write_SDTS_cats (map, sdts_prefix, sdts_path, layer_name))
	  stat = 0;

	/*
	fprintf (stderr, "Writing STAT Module\n");
	write_SDTS_stat (map, sdts_prefix, sdts_path);
	*/

	fprintf (stderr, "Writing SPDM Module\n");
	if (!write_SDTS_spdm (map, sdts_prefix, sdts_path))
	  stat = 0;

	fprintf (stderr, "Writing AP00 Module\n");
	if (!write_SDTS_global_atts (map, sdts_prefix, sdts_path, zone, ll_input))
	  stat = 0;

	fprintf (stderr, "Writing CATD Module\n");
	if (!write_SDTS_catd (map, sdts_prefix, sdts_path))
	  stat = 0;

	return (stat);
}

write_SDTS_iden (map, sdts_prefix, sdts_path)
  struct Map_info *map;
  char *sdts_prefix;
  char *sdts_path;

{
    char iden_base [30];
	char id1_str [500];
	char id2_str [500];
	char iden_str [1000];
	char conf_str [200];
	char atid_str [100];
    char rcid_str[10];
    FILE *fpout;


	Mod[IDEN].rec_cnt = 0;

    strcpy (iden_base, sdts_prefix);
    strcpy (iden_base + 4, "IDEN.DDF");

	if (!Open_sdtsfile (sdts_path, iden_base, &fpout, 'W'))
         return (0);



	if (!begin_sdts_ddr (iden_base, fpout))
	{
	    end123file(&fpout);
		return (0);
	}


    write_dd_fld (iden_base, fpout, "IDEN", "1600;&IDENTIFICATION","MODN!RCID!STID!STVS!PRID!PRVS!PDOC!TITL!DAST!MPDT!DCDT!SCAL!COMT", "(A, I, 9A, I, A)", 6);

    write_dd_fld (iden_base, fpout, "CONF", "1600;&CONFORMANCE","FFYN!VGYN!GTYN!RCYN!EXSP!FTLV", "(4A, 2I)", 6);

    write_dd_fld (iden_base, fpout, "ATID", "1600;&ATTRIBUTE ID","MODN!RCID", "(A, I)", 3);

	if ( (get_err_stat() != 1) || (!end_dd_rec(iden_base, fpout)))
	{
	    end123file(&fpout);
		return (0);
	}

	if (!begin_data_rec (iden_base, fpout))
	{
	    end123file(&fpout);
		return (0);
	}

		 sprintf (rcid_str, "%6d", 1);
		 sprintf (id1_str, "%4s\037%6d\037%s\037%s\037%s\037%s\037%s",
		   Mod[IDEN].name, 
		   1,
		   IDEN_STID,
		   IDEN_STVS,
		   IDEN_PRID,
		   IDEN_PRVS,
		   IDEN_PDOC);

/*DEBUG*//* fprintf (stderr, "id1_str= '%s'\n", id1_str); */

		 sprintf (id2_str, "\037%s\037%s\037%s\037%s\037%i\037%s",
		   Iden_titl, 
		   get_IDEN_DAST (map),
		   Iden_mpdt, 
		   get_IDEN_DCDT (), 
		   map->head.orig_scale, 
		   Iden_comt );

/*DEBUG*//* fprintf (stderr, "id2_str= '%s\n", id2_str); */

		 sprintf (iden_str, "%s%s", id1_str, id2_str);

/*DEBUG*/ /*fprintf (stderr, "iden_str= '%s\n", iden_str);*/

		 sprintf (conf_str, "%s\037%s\037%s\037%s\037%d\037%d",
		   "N",
		   "N",
		   "Y",
		   "N" ,
			1,
			4); 

		 sprintf (atid_str, "%s\037%d",
		   "AP00",  1);

		 write_data_fld (iden_base, fpout, DDF_ID, LEAD_ID, rcid_str, RCID_LEN, 2); 

		 write_data_fld (iden_base, fpout, "IDEN", LEAD_ID, iden_str, strlen (iden_str), 6);
		 write_data_fld (iden_base, fpout, "CONF", LEAD_ID, conf_str, strlen (conf_str), 6);
		 write_data_fld (iden_base, fpout, "ATID", LEAD_ID, atid_str, strlen (atid_str), 4);

		 Mod[IDEN].rec_cnt++;

		if ( (get_err_stat() != 1) || (!end_data_rec(iden_base, fpout)))
		{
			end123file(&fpout);
		    Mod[IDEN].rec_cnt = 0;
			return (0);
		}

	if (end_sdtsfile (iden_base, &fpout))
	   return (1);
    else
	{
		Mod[IDEN].rec_cnt = 0;
		return (0);
	}
}

write_SDTS_iref (map, sdts_prefix, sdts_path)
  struct Map_info *map;
  char *sdts_prefix;
  char *sdts_path;

{
    char iref_base [30];
	char ir1_str [450];
	char ir2_str [450];
	char iref_str [900];
    char rcid_str[10];
    FILE *fpout;


	Mod[IREF].rec_cnt = 0;

    strcpy (iref_base, sdts_prefix);
    strcpy (iref_base + 4, "IREF.DDF");

	if (!Open_sdtsfile (sdts_path, iref_base, &fpout, 'W'))
         return (0);


	if (!begin_sdts_ddr (iref_base, fpout))
	{
	    end123file(&fpout);
		return (0);
	}


    if (!write_dd_fld (iref_base, fpout, "IREF", "1600;&INTERNAL SPATIAL REFERENCE","MODN!RCID!SATP!XLBL!YLBL!HFMT!SFAX!SFAY!XORG!YORG", "(A, I, 4A, 4R)", 3))
    {
	    end123file(&fpout);
		return (0);
	}
	if (!end_dd_rec(iref_base, fpout))
    {
	    end123file(&fpout);
		return (0);
	}
	if (!begin_data_rec (iref_base, fpout))
    {
	    end123file(&fpout);
		return (0);
	}

	sprintf (rcid_str, "%6d", 1);

	sprintf (ir1_str, "%s\037%d\037%s\037%s\037%s\037%s\037",
	       Mod[IREF].name,
		   1, 
		   IREF_SATP,
		   Iref_Xlbl,
		   Iref_Ylbl,
		   IREF_HFMT);

	sprintf (ir2_str, "%8.6f\037%8.6f\037%3.1f\037%3.1f",
		   Sfax,
		   Sfay,
		   IREF_XORG,
		   IREF_YORG
		   );

	 sprintf (iref_str, "%s%s", ir1_str, ir2_str);


	 write_data_fld (iref_base, fpout, DDF_ID, LEAD_ID, rcid_str, RCID_LEN, 2); 

	 write_data_fld (iref_base, fpout, "IREF", LEAD_ID, iref_str, strlen (iref_str), 4);

	 Mod[IREF].rec_cnt++;

	if ( (get_err_stat() != 1) || (!end_data_rec(iref_base, fpout)))
    {
		end123file(&fpout);
	    Mod[IREF].rec_cnt = 0;
		return (0);
	}

	if (end_sdtsfile (iref_base, &fpout))
	   return (1);
    else
	{
		Mod[IREF].rec_cnt = 0;
		return (0);
	}
}

write_SDTS_xref (map, sdts_prefix, sdts_path)
  struct Map_info *map;
  char *sdts_prefix;
  char *sdts_path;

{
    char xref_base [30];
	char xref_str [500];
    char rcid_str[10];
    FILE *fpout;

	Mod[XREF].rec_cnt = 0;

    strcpy (xref_base, sdts_prefix);
    strcpy (xref_base + 4, "XREF.DDF");

	if (!Open_sdtsfile (sdts_path, xref_base, &fpout, 'W'))
    {
         fprintf (stderr, "couldn't open xref module outputfile.\n");
         return (0);
    };

	if (!begin_sdts_ddr (xref_base, fpout))
	{
	    end123file(&fpout);
		return (0);
	}


    if (!write_dd_fld (xref_base, fpout, "XREF", "1600;&EXTERNAL SPATIAL REFERENCE","MODN!RCID!RSNM!HDAT!ZONE", "(A, I, 2A, A)", 3))
	{
	    end123file(&fpout);
		return (0);
	}
    
	if (!end_dd_rec(xref_base, fpout))
	{
	    end123file(&fpout);
		return (0);
	}

	if (!begin_data_rec (xref_base, fpout))
	{
	    end123file(&fpout);
		return (0);
	}

		 sprintf (rcid_str, "%6d", 1);
	sprintf (xref_str, "%s\037%d\037%s\037%s\037%s",
	       Mod[XREF].name,
		   1,
		   Xref_rsnm,
		   Xref_hdat,
		   Zone_str);


   write_data_fld (xref_base, fpout, DDF_ID, LEAD_ID_R, rcid_str, RCID_LEN, 2); 

   write_data_fld (xref_base, fpout, "XREF", LEAD_ID_R, xref_str, strlen (xref_str), 4);

    Mod[XREF].rec_cnt++;

	if ( (get_err_stat() != 1) || (!end_data_rec(xref_base, fpout)))
    {
		end123file(&fpout);
	    Mod[XREF].rec_cnt = 0;
		return (0);
	}

	if (end_sdtsfile (xref_base, &fpout))
	   return (1);
    else
	{
		Mod[XREF].rec_cnt = 0;
		return (0);
	}
}

write_SDTS_spdm (map, sdts_prefix, sdts_path)
  struct Map_info *map;
  char *sdts_prefix;
  char *sdts_path;

{
    char spdm_base [30];
	char spdm_str [500];
    char rcid_str[10];
    FILE *fpout;


	Mod[SPDM].rec_cnt = 0;

    strcpy (spdm_base, sdts_prefix);
    strcpy (spdm_base + 4, "SPDM.DDF");

	if (!Open_sdtsfile (sdts_path, spdm_base, &fpout, 'W'))
         return (0);


	if (!begin_sdts_ddr (spdm_base, fpout))
	{
	    end123file(&fpout);
		return (0);
	}


    write_dd_fld (spdm_base, fpout, "SPDM", "1600;&SPATIAL DOMAIN","MODN!RCID!DTYP!DSTP", "(A, I, 2A)", 6);

    write_dd_fld (spdm_base, fpout, "DMSA", "2600;&DOMAIN SPATIAL ADDRESS","*X!Y", "((2B(32)))", 3);

	if ( (get_err_stat() != 1) || (!end_dd_rec(spdm_base, fpout)))
	{
	    end123file(&fpout);
		return (0);
	}

	if (!begin_data_rec (spdm_base, fpout))
	{
	    end123file(&fpout);
		return (0);
	}

	sprintf (rcid_str, "%6d", 1);

	sprintf (spdm_str, "%s\037%d\037%s\037%s",
	       Mod[SPDM].name,
		   1,
		   SPDM_DTYP,
		   SPDM_DSTP);


	 write_data_fld (spdm_base, fpout, DDF_ID, LEAD_ID_R, rcid_str, RCID_LEN, 2); 

	 write_data_fld (spdm_base, fpout, "SPDM", LEAD_ID_R, spdm_str, strlen (spdm_str), 6);

	 write_coords (fpout, map->head.W, map->head.S, 0, LEAD_ID_R, 6, 1, "DMSA");
	 write_coords (fpout, map->head.E, map->head.N, 0, LEAD_ID_R, 1, 4, "DMSA");

	 Mod[SPDM].rec_cnt++;

	if ( (get_err_stat() != 1) || (!end_data_rec(spdm_base, fpout)))
    {
		end123file(&fpout);
	    Mod[SPDM].rec_cnt = 0;
		return (0);
	}

	if (end_sdtsfile (spdm_base, &fpout))
	   return (1);
    else
	{
		Mod[SPDM].rec_cnt = 0;
		return (0);
	}
}

write_SDTS_catd (map, sdts_prefix, sdts_path)
  struct Map_info *map;
  char *sdts_prefix;
  char *sdts_path;

{
    char catd_base [30];
    char rcid_str[250];
    char rec_str[250];
	char file_str[250];
    FILE *fpout;
	int i;

    Mod[CATD].rec_cnt = 0;

    strcpy (catd_base, sdts_prefix);
    strcpy (catd_base + 4, "CATD.DDF");

	if (!Open_sdtsfile (sdts_path, catd_base, &fpout, 'W'))
         return (0);

	if (!begin_sdts_ddr (catd_base, fpout))
	{
		end123file (&fpout);
		return (0);
	}


     if (!write_dd_fld (catd_base, fpout, "CATD", "1600;&CATALOG/DIRECTORY","MODN!RCID!NAME!TYPE!FILE", "(A(4), I(6), A(4), A(30), A(12) )", 3))
	 {
		end123file (&fpout);
		return (0);
	 }

	 if (!end_dd_rec (catd_base, fpout))
	 {
		end123file (&fpout);
		return (0);
	 }

	 Mod[CATD].rec_cnt = 0;

	 for (i = 1; Mod[i].name != NULL; i++)
	 {
		 if (( !Mod[i].rec_cnt ) && strcmp (Mod[i].name, "STAT"))
		   {
		      if (!strncmp (Mod[i].name, "NE", 2)
			||!strncmp (Mod[i].name, "NA", 2)
			||!strncmp (Mod[i].name, "NP", 2)
			||!strncmp (Mod[i].name, "NL", 2)
           ||!strcmp (Mod[i].name, "SCUR")
/*
            ||!strcmp (Mod[i].name, "DDDF")
*/
            ||!strcmp (Mod[i].name, "SPDM"))
                        continue;
		      else
			fprintf (stderr, "%s module should have at least one data record\n", Mod[i].name);
		   }
		 if (!begin_data_rec (catd_base, fpout))
		 {
             Mod[CATD].rec_cnt = 0;
			 end123file (&fpout);
			 return (0);
		 }

         Mod[CATD].rec_cnt++;

		 sprintf (rcid_str, "%6d", Mod[CATD].rec_cnt);
		 sprintf (file_str, "%s%s.DDF", sdts_prefix, Mod[i].name);
		 sprintf (rec_str, "%4s%6d%-4s%-30s%-12s",
		   Mod[CATD].name,
		   Mod[CATD].rec_cnt,
		   Mod[i].name,
		   Mod[i].type,
		   file_str);

		write_data_fld (catd_base, fpout, DDF_ID, LEAD_ID_R, rcid_str, RCID_LEN, 2); 

		if (Mod[i+1].name == NULL)
			write_data_fld (catd_base, fpout, Mod[CATD].name, LEAD_ID_R, rec_str, strlen (rec_str), 4);
        else
			write_data_fld (catd_base, fpout, Mod[CATD].name, LEAD_ID_R, rec_str, strlen (rec_str), 3);


	    if ( (get_err_stat() != 1) || (!end_data_rec(catd_base, fpout)))
        {
		    end123file(&fpout);
	        Mod[CATD].rec_cnt = 0;
		    return (0);
	    }

	}

	if (end_sdtsfile (catd_base, &fpout))
	   return (1);
    else
	{
		Mod[CATD].rec_cnt = 0;
		return (0);
	}
}

write_SDTS_catx (map, sdts_prefix, sdts_path)
  struct Map_info *map;
  char *sdts_prefix;
  char *sdts_path;

{
    char catx_base [30];
    char rcid_str[250];
    char rec_str[250];
	char vol_name[10];
    FILE *fpout;
	int i, j, recnum;

    Mod[CATX].rec_cnt = 0;

    strcpy (catx_base, sdts_prefix);
    strcpy (catx_base + 4, "CATX.DDF");

	if (!Open_sdtsfile (sdts_path, catx_base, &fpout, 'W'))
         return (0);

    sprintf (vol_name, "%sSDTS", sdts_prefix);

	if (!begin_sdts_ddr (catx_base, fpout))
	{
		end123file (&fpout);
		return (0);
	}


    if (!write_dd_fld (catx_base, fpout, "CATX", "1600;&CATALOG/CROSS-REFERENCE","MODN!RCID!NAM1!TYP1!NAM2!TYP2", "(A(4), I(6), A(4), A(30), A(4), A(30) )", 3))
	{
		end123file (&fpout);
		return (0);
	}

	if (!end_dd_rec(catx_base, fpout))
	{
		end123file (&fpout);
		return (0);
	}

	 recnum = 0;

	 for (i = 1; Mod[i].name != NULL; i++)
	 {
		 for (j=1; Mod[i].xref[j] != 0; j++)
		 {
			 if (!begin_data_rec (catx_base, fpout))
			 {
				end123file (&fpout);
				return (0);
			 }

			 sprintf (rcid_str, "%6d", ++recnum);
			 sprintf (rec_str, "%4s%6d%-4s%-30s%-4s%-30s",
			   Mod[CATX].name,
			   recnum,
			   Mod[i].name,
			   Mod[i].type,
			   Mod[Mod[i].xref[j]].name,
			   Mod[Mod[i].xref[j]].type);

			write_data_fld (catx_base, fpout, DDF_ID, LEAD_ID_R, rcid_str, RCID_LEN, 2); 

			if ((Mod[i+1].name == NULL) && (Mod[i].xref[j+1] == 0))  
				write_data_fld (catx_base, fpout, Mod[CATX].name, LEAD_ID_R, rec_str, strlen (rec_str), 4);
			else
				write_data_fld (catx_base, fpout, Mod[CATX].name, LEAD_ID_R, rec_str, strlen (rec_str), 3);

		    Mod[CATX].rec_cnt++;

	        if ( (get_err_stat() != 1) || (!end_data_rec(catx_base, fpout)))
            {
		       end123file(&fpout);
	           Mod[CATX].rec_cnt = 0;
		       return (0);
	        }
		 }
	 }

	if (end_sdtsfile (catx_base, &fpout))
	   return (1);
    else
	{
		Mod[CATX].rec_cnt = 0;
		return (0);
	}

}

write_SDTS_cats (map, sdts_prefix, sdts_path, layer_name)
  struct Map_info *map;
  char *sdts_prefix;
  char *sdts_path;
  char *layer_name;
{
    char cats_base [30];
    char rcid_str[250];
    char rec_str[250];
	char vol_name[10];
    FILE *fpout;
	int i, recnum;

    Mod[CATS].rec_cnt = 0;

    strcpy (cats_base, sdts_prefix);
    strcpy (cats_base + 4, "CATS.DDF");

	if (!Open_sdtsfile (sdts_path, cats_base, &fpout, 'W'))
         return (0);

    sprintf (vol_name, "%sSDTS", sdts_prefix);

	if (!begin_sdts_ddr (cats_base, fpout))
	{
		end123file (&fpout);
		return (0);
    }

    if (!write_dd_fld (cats_base, fpout, "CATS", "1600;&CATALOG/SPATIAL DOMAIN","MODN!RCID!NAME!TYPE!MAP!THEM!AGOB!AGTP", "(A, I, 6A)", 3))
	{
		end123file (&fpout);
		return (0);
	}

	if (!end_dd_rec (cats_base, fpout))
	{
		end123file (&fpout);
		return (0);
	}

	 recnum = 0;

	 for (i = 1; Mod[i].name != NULL; i++)
	 {
           if ( !Mod[i].rec_cnt )  
               if  ( !strncmp (Mod[i].name, "NE", 2)
                || !strncmp (Mod[i].name, "NA", 2)
                || !strncmp (Mod[i].name, "NP", 2)
                || !strncmp (Mod[i].name, "NL", 2) ) 
               continue; /*skip over empty object modules*/

			 if (!begin_data_rec (cats_base, fpout))
			 {
				end123file (&fpout);
		        Mod[CATS].rec_cnt = 0;
				return (0);
			 }

			 sprintf (rcid_str, "%6d", ++recnum);
			 sprintf (rec_str, "%s\037%d\037%s\037%s\037%s\037%s\037%s\037%s",
			   Mod[CATS].name,
			   recnum,
			   Mod[i].name,
			   Mod[i].type,
			   G_myname(),
			   layer_name,
			   "", /*for missing AGOB */
			   "GT");

			write_data_fld (cats_base, fpout, DDF_ID, LEAD_ID, rcid_str, RCID_LEN, 2); 

			if (Mod[i+1].name == NULL)  
				write_data_fld (cats_base, fpout, Mod[CATS].name, LEAD_ID, rec_str, strlen (rec_str), 4);
			else
				write_data_fld (cats_base, fpout, Mod[CATS].name, LEAD_ID, rec_str, strlen (rec_str), 3);

		    Mod[CATS].rec_cnt++;

	        if ( (get_err_stat() != 1) || (!end_data_rec(cats_base, fpout)))
            {
		       end123file(&fpout);
	           Mod[CATS].rec_cnt = 0;
		       return (0);
	        }
	 }

	if (end_sdtsfile (cats_base, &fpout))
	   return (1);
    else
	{
		Mod[CATS].rec_cnt = 0;
		return (0);
	}
}

write_SDTS_stat (map, sdts_prefix, sdts_path)
  struct Map_info *map;
  char *sdts_prefix;
  char *sdts_path;

{
    char stat_base [30];
    char rcid_str[250];
    char rec_str[250];
	char file_str[250];
	char vol_name[10];
    FILE *fpout;
	int i, stat_rec = 0;

	fprintf (stderr, "Writing STAT Module\n");

	Mod[STAT].rec_cnt = 0;

    strcpy (stat_base, sdts_prefix);
    strcpy (stat_base + 4, "STAT.DDF");

	if (!Open_sdtsfile (sdts_path, stat_base, &fpout, 'W'))
         return (0);

    sprintf (vol_name, "%sSDTS", sdts_prefix);

	if (!begin_sdts_ddr (stat_base, fpout))
	{
		end123file (&fpout);
		return (0);
    }


/*before processing records, set number of records for STATS module itself*/

    if (!write_dd_fld (stat_base, fpout, "STAT", "1600;&TRANSFER STATISTICS","MODN!RCID!MNTF!MNRF!NREC!NSAD", "(A(4), I(6), A(30), A(4), I(6), I(10))", 3))
	{
		end123file (&fpout);
		return (0);
    }

	if (!end_dd_rec(stat_base, fpout))
	{
		end123file (&fpout);
		return (0);
    }

	 /* before creating record strings, get rec_cnt for STATs module*/ 
	 for (i = 1; Mod[i].name != NULL; i++)
	 {
		 if (Mod[i].rec_cnt > 0) /*don't count empty modules*/
			 stat_rec++;
     }
     /*add one more record for STAT module itself*/
	 Mod[STAT].rec_cnt = stat_rec + 1;

	 /* now create record strings and records*/
	 for (i =  stat_rec = 1; Mod[i].name != NULL; i++)
	 {
	    if (Mod[i].rec_cnt)
		{
		    if (!begin_data_rec (stat_base, fpout))
			{
	            Mod[STAT].rec_cnt = 0;
				end123file (&fpout);
				return (0);
			}

		 sprintf (rcid_str, "%6d", stat_rec);
		 sprintf (file_str, "%s%s.DDF", sdts_prefix, Mod[i].name);
		 sprintf (rec_str, "%4s%6d%-30s%-4s%6ld%10ld",
		   Mod[STAT].name,
		   stat_rec++,
		   Mod[i].type,
		   Mod[i].name,
		   Mod[i].rec_cnt,
		   Mod[i].sadr_cnt);

		write_data_fld (stat_base, fpout, DDF_ID, LEAD_ID_R, rcid_str, RCID_LEN, 2); 

		if (Mod[i+1].name == NULL)
			write_data_fld (stat_base, fpout, Mod[STAT].name, LEAD_ID_R, rec_str, strlen (rec_str), 4);
        else
			write_data_fld (stat_base, fpout, Mod[STAT].name, LEAD_ID_R, rec_str, strlen (rec_str), 3);


	        if ( (get_err_stat() != 1) || (!end_data_rec(stat_base, fpout)))
            {
		       end123file(&fpout);
	           Mod[STAT].rec_cnt = 0;
		       return (0);
	        }
		}
	 }

	if (end_sdtsfile (stat_base, &fpout))
	   return (1);
    else
	{
		Mod[STAT].rec_cnt = 0;
		return (0);
	}
}
	
write_SDTS_global_atts (map, sdts_prefix, sdts_path, zone, ll_input)
  struct Map_info *map;
  char *sdts_prefix;
  char *sdts_path;
  int zone, ll_input;

{
    char gl_atts_base [30];
    char tmp_str[250];
    char tmp2_str[250];
	char vol_name[10];
    FILE *fpout;
    int n_int_lat, s_int_lat, e_int_lon, w_int_lon;

    Mod[AP00].rec_cnt = 0;

    strcpy (gl_atts_base, sdts_prefix);
    strcpy (gl_atts_base + 4, "AP00.DDF");

	if (!Open_sdtsfile (sdts_path, gl_atts_base, &fpout, 'W'))
         return (0);

    sprintf (vol_name, "%sSDTS", sdts_prefix);

	if (!begin_sdts_ddr (gl_atts_base, fpout))
	{
	    end123file(&fpout);
		return (0);
	}


/*before processing records, set number of records for STATS module itself*/

     write_dd_fld (gl_atts_base, fpout, "ATPR", "1600;&ATTRIBUTE PRIMARY","MODN!RCID", "(A, I)", 6);

     write_dd_fld (gl_atts_base, fpout, "ATTP", "1600;&PRIMARY ATTRIBUTES","ORGANIZATION!DIGIT_DATE!DIGIT_NAME!MAP_NAME!MAP_DATE!OTHER_INFO!ZONE!WEST_EDGE!EAST_EDGE!SOUTH_EDGE!NORTH_EDGE!DIGIT_THRESH!MAP_THRESH!VERSION!BACK_VERSION!ELLIPSOID", "(A, A, A, A, A, A, I, 4R, 4R, A)", 3);

	if ( (get_err_stat() != 1) || (!end_dd_rec(gl_atts_base, fpout)))
	{
	    end123file(&fpout);
		return (0);
	}

    if (!begin_data_rec (gl_atts_base, fpout))
	{
        Mod[AP00].rec_cnt = 0;
		end123file (&fpout);
		return (0);
	}

	 sprintf (tmp_str, "%6d", 1);
	 write_data_fld (gl_atts_base, fpout, DDF_ID, LEAD_ID, tmp_str, RCID_LEN, 2); 

	 sprintf (tmp_str, "%4s\037%6d", Mod[AP00].name,  1);
	 write_data_fld (gl_atts_base, fpout, "ATPR", LEAD_ID, tmp_str, (long) strlen (tmp_str), 6);

     sprintf (tmp_str, "%-29s\037%-19s\037%-19s\037%s\037%s\037%s",
		   map->head.organization,
		   map->head.date,
		   map->head.your_name,
		   map->head.map_name,
		   map->head.source_date,
		   map->head.line_3);


/*use bounding box NSEW instead of header info for global_atts NSEW*/
#ifndef NO_CONVERSION
     if (!ll_input)
     {
         utm_to_ll (map->Line[1].W, map->Line[1].N, &n_int_lat, &w_int_lon, zone);
         utm_to_ll (map->Line[1].E,  map->Line[1].S, &s_int_lat, &e_int_lon, zone);
         sprintf (tmp2_str, "\037%4d\037%10ld\037%10ld\037%10ld\037%10ld\037%10f\037%10f",
               map->head.plani_zone,
               w_int_lon,
               e_int_lon,
               s_int_lat,
               n_int_lat,
               map->head.digit_thresh,
               map->head.map_thresh);
     }
     else
#endif /* NO_CONVERSION */

     {
     sprintf (tmp2_str, "\037%4d\037%10f\037%10f\037%10f\037%10f\037%10f\037%10f\037%d.%d\037%d.%d\037%s",
		   map->head.plani_zone,
		   map->head.W,
		   map->head.E,
		   map->head.S,
		   map->head.N,
		   map->head.digit_thresh,
		   map->head.map_thresh,
		   map->head.Version_Major,
		   map->head.Version_Minor,
		   map->head.Back_Major,
		   map->head.Back_Minor,
		   AP00_ellps);
     }

	 strcat (tmp_str, tmp2_str);

     write_data_fld (gl_atts_base, fpout, "ATTP", LEAD_ID, tmp_str, strlen (tmp_str), 4);

	 Mod[AP00].rec_cnt++;

	 if (!end123rec (fpout)){
		fprintf (stderr, "unable to end Data Record");
		end123file (&fpout);
	 }

	 if ( (get_err_stat() != 1) || (!end_data_rec(gl_atts_base, fpout)))
     {
	      end123file(&fpout);
	      Mod[AP00].rec_cnt = 0;
	      return (0);
	 }

	 if (end_sdtsfile (gl_atts_base, &fpout))
	    return (1);
     else
	 {
	 	Mod[AP00].rec_cnt = 0;
		return (0);
	 }
}



write_SDTS_quality_mods (sdts_prefix, sdts_path, layer_name)
  char *sdts_prefix;
  char *sdts_path;
  char *layer_name;
{

	int stat = 1;

	fprintf (stderr, "Writing DQHL Module\n");
	if (!write_quality_mod (sdts_prefix, sdts_path, DQHL, layer_name))
	{
	   fprintf (stderr, "Error: %s\n", get_err_mess());
	   stat = 0;
	   clear_err_mess();
	}
	else
	   Mod[DQHL].rec_cnt = 1;

	fprintf (stderr, "Writing DQPA Module\n");
	if (!write_quality_mod (sdts_prefix, sdts_path, DQPA, layer_name))
	{
	   fprintf (stderr, "Error: %s\n", get_err_mess());
	   stat = 0;
	   clear_err_mess();
	}
	else
	   Mod[DQPA].rec_cnt = 1;

	fprintf (stderr, "Writing DQAA Module\n");
	if (!write_quality_mod (sdts_prefix, sdts_path, DQAA, layer_name))
	{
	   fprintf (stderr, "Error: %s\n", get_err_mess());
	   stat = 0;
	   clear_err_mess();
	}
	else
	   Mod[DQAA].rec_cnt = 1;

	fprintf (stderr, "Writing DQLC Module\n");
	if (!write_quality_mod (sdts_prefix, sdts_path, DQLC, layer_name))
	{
	   fprintf (stderr, "Error: %s\n", get_err_mess());
	   stat = 0;
	   clear_err_mess();
	}
	else
	   Mod[DQLC].rec_cnt = 1;

	fprintf (stderr, "Writing DQCG Module\n");
	if (!write_quality_mod (sdts_prefix, sdts_path, DQCG, layer_name))
	{
	   fprintf (stderr, "Error: %s\n", get_err_mess());
	   stat = 0;
	   clear_err_mess();
	}
	else
	   Mod[DQCG].rec_cnt = 1;
	
	return (stat);
}


