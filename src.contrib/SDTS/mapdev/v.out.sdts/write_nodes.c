/*
**  Written by David Stigberg 1994, derived from  Dave Gerdes v.out.dlg code
**  US Army Construction Engineering Research Lab
*/
#include "Vect.h"

#include "stc123.h"
#include "defines.h"
#include "externs.h"
#include "globals2.h"


char * get_err_mess();

#define SDTS_ICE   " "
#define SDTS_LEVEL 2

/*
**  node atts are currenty not supported in digit, but this code should
**  work when it is.
**
*/

/*   SDTS Planar Node Module Schema
**   ------------------------------
**
**    PNTS  MODN       4    A   module name: NO01      
**    PNTS  RCID       6    I   record id 
**    PNTS  OBRP       2    A   obj representation code: NO
**
**    SADR  X         10    I   longitude
**    SADR  y         10    I   latitude
**
**    ATID  MODN       4    A   attribute module name 
**    ATID  RCID       6    I   attrib primary id
**
*/

/*SDTS PLANAR NODE DEFINES*/

#define DDF_ID "0001"
#define MODN   "NO01"
#define OBRP   "NO"
#define LEAD_ID 'D'
#define LEAD_ID_R 'R'


write_SDTS_nodes (map, full_fname, fname, ll_input, zone)
    struct Map_info *map;
	char *full_fname;
	char *fname;
	int ll_input, zone;
{
    FILE *fpout;
    P_NODE *Node;
    int n_atts;
    int n_lines;
    register int node, i, cnt;
	char rcid_str[40];
	char rcid_str2[40];
	char att_str [20];
	int got_real_node;
	int got_entity_point;
	int latest_rec_has_atid;
	int node_att;
	int option;
/*	int num_dots_on_node;*/ /*DEBUG*/


/*OPEN SDTS PLANAR NODE OUTPUT FILE*/
	if (!open_sdtsfile (full_fname, &fpout, 'W'))
		 return (0);

/*BEGIN OUTPUT: WRITE DDR RECORD*/
    
	if (!begin_sdts_ddr (fname, fpout))
	{
		end_dd_rec (fpout);
		end123file (&fpout);
		return (0);
    }

    write_dd_fld (fname, fpout,"PNTS","1600;&POINT-NODE","MODN!RCID!OBRP","(A(4),I(6),A(2))",6);
	
	write_dd_fld (fname, fpout, "SADR", "1600;&SPATIAL ADDRESS", "X!Y", "(2B(32))", 6);	 

/*INCLUDE: ATTR POINTER FOR PLANAR NODES CONVERTED FROM ENTITY POINTS*/
	write_dd_fld (fname, fpout, "ATID", "1600;&ATTRIBUTE ID", "MODN!RCID", "(A(4),I(6))", 3); 	 


	if ((get_err_stat()) != 1)
	{
		end_dd_rec (fname, fpout);
		end123file (&fpout);
		return (0);
	}

	if (!end_dd_rec(fname, fpout))
	{
		end123file (&fpout);
		return (0);
	}

/*BEGIN PROCESSING ACTUAL DATA*/

	/*initialize sdts rec ctr*/
	cnt = 0;

    for (node = 1 ; node <= map->n_nodes ; node++)
    {

		n_atts = 0;
		n_lines = 0;
		got_real_node = 0;
		got_entity_point = 0;
		node_att = 0;
		/*num_dots_on_node = 0;*/ /*DEBUG*/

		Node = &(map->Node[node]);


		/*If the node is a non-DOT node: write it out*/
		/*Plus, if it is both a non-DOT and a DOT node: 
		get the DOT attribute and write it out with the node*/

		for (i = 0 ; i < Node->n_lines ; i++)
		{
			char ltype = map->Line[abs (Node->lines[i])].type;

			if ((ltype == AREA)  || (ltype == LINE && !Aline_only))
				got_real_node = 1;
			else 
			  if (ltype == DOT) 
			  {
			  	got_entity_point = 1;  /*DOT node*/
			  	/*this will actually get the same node_att twice*/
			  	node_att = map->Line[abs (Node->lines[i])].att;
				/*DEBUG*//* num_dots_on_node++;*/
			  }
		}
		/*DEBUG*//* fprintf (stderr, "#%d Dots on node: %d\n", node, num_dots_on_node);*/
		if (!got_real_node) /*if it's only a DOT node, skip it*/
		{
		   /*make sure this isn't the final record. If so, we have to backup
		   and re-write the last subfield of the last legit record with 
		   EOF option, then break*/
		   /*plus, if we do backup, do we re-write coords or atts?*/

		    if (node == map->n_nodes && cnt > 0) 
			{
			   if (latest_rec_has_atid)
			   {
				   if (!backup_and_write_atid (fpout, LEAD_ID, att_str))
				   {
					 end123file (&fpout);
					 return (0);
				   }
			   }
               else
			   {
				   if (!backup_and_write_coords (fpout, LEAD_ID))
				   {
					 end123file (&fpout);
					 return (0);
				   }
			   }
			   if (!end_data_rec (fname, fpout))
			   {
				   end123file (&fpout);
				   return (0);
			   }
			   break;
			}
			else
		      continue;
		}

		/*increment counter for sdts module id*/
		/*but use node as actual rec id*/

		cnt++;

		/*BEGIN SDTS RECORD PROCESSING*/
		if (!begin_data_rec (fname, fpout))
		{
		   end123file (&fpout);
		   return (0);
		}

	/* count how many atts will have */
#ifdef FOO
		for (i = 0 ; i < Node->n_lines ; i++)
		{
			if (map->Line[abs (Node->lines[i])].type == DOT)
			{
			/* only get the positive lines.  remember that both
			** ends of the DOT line will come into this node 
			*/
				if (Node->lines[i] > 0 && map->Line[Node->lines[i]].att)
					n_atts++;
			}
			else
				n_lines++;
		}
#endif FOO


		 sprintf (rcid_str, "%6d", cnt);

		 sprintf (rcid_str2, "%4s%6d%2s", MODN,  node,  OBRP );

		 write_data_fld (fname, fpout, DDF_ID, LEAD_ID, rcid_str, (long) strlen (rcid_str), 2);

		 write_data_fld (fname, fpout, "PNTS", LEAD_ID, rcid_str2, (long) strlen (rcid_str2), 6);

		 if (node == map->n_nodes)       /*use EOF option = 4 */
               option = 4;
         else
			   option = 3;

		 if (node_att)
         {
			latest_rec_has_atid = 1;
            sprintf (att_str, "%4s%6d", "AP01", node_att);
		      /*SADRs and ATTR*/
			write_NO_coords (fpout, Node->x, Node->y, zone, 5);

			write_data_fld (fname, fpout, "ATID", LEAD_ID, att_str, (long) strlen (att_str), option);
		 }
		 else
		     /*SADRs only */
         {
			latest_rec_has_atid = 0;
			write_NO_coords (fpout, Node->x, Node->y, zone, option);
         }

         /*CHECK FOR WRITE ERRORS*/
		 if (get_err_stat() != 1)
		 {
			 end123file (&fpout);
		     Mod[NO01].sadr_cnt = 0;
		     Mod[NO01].rec_cnt = 0;
			 return (0);
		 }
		 else
		 {
			 Mod[NO01].sadr_cnt++;
			 Mod[NO01].rec_cnt++;
		 }

#ifdef TEST_VER
          if (Mod[NO01].rec_cnt%10 == 0)
                fprintf (stderr, " writing %dth record\n", Mod[NO01].rec_cnt);
#endif

	    if (!end_data_rec (fname, fpout)){
	        Mod[NO01].sadr_cnt = 0;
		    Mod[NO01].rec_cnt = 0;
			end123file (&fpout);
			return (0);
        }


    }
	if (!end_sdtsfile (fname, &fpout))
	{
	  Mod[NO01].sadr_cnt = 0;
	  Mod[NO01].rec_cnt = 0;
	  return (0);
	}
	else
	  return (1);
}

backup_and_write_atid (fpout, lead_id, att_str)
    FILE *fpout;
	int lead_id;
	char *att_str;
{
	 int bkstat;

/*backup final field of final record and rewrite with EOF option*/

     if (!bak123fld (fpout, &bkstat)) 
	 {
	    put_err_mess ("Failure backing up and writing ATID.", 0);
        return (0);
     }

	if (!wr123fld (fpout, "ATID", lead_id, att_str, (long) strlen (att_str), 4))
	{
		put_err_mess ("Failed to write 123 Data Field after backing up ATID field.", 0);
		return (0);
	}
    
	return (1);
}
