#include "Vect.h"

/* routines for writing Dig+ structures to ascii. */
/* return 0 on success, -1 on failure of whatever kind */

int Wr_P_node_asc ( int  num , struct P_node *ptr, FILE *fp)
{
    int  i ;

    fprintf( fp, "%4d  x: %10.2f,  y: %10.2f,  n_lines: %d \n",
	num, ptr->x, ptr->y, ptr->n_lines) ;

    for ( i=0; i < ptr->n_lines; i++)
    {
	fprintf( fp, "     lines,     angles");
	fprintf( fp, "      %4d,        %f\n", ptr->lines[i], ptr->angles[i]);
    }
    return (0);
}

/*
char codes[] = { "ALP alp" };
*/

char 
codes (int type)
{
    switch (type) {
	case LINE:
	    return ('L');
	    break;
	case AREA:
	    return ('A');
	    break;
	case DOT:
	    return ('P');
	    break;
	case DEAD_LINE:
	    return ('l');
	    break;
	case DEAD_AREA:
	    return ('a');
	    break;
	case DEAD_DOT:
	    return ('p');
	    break;
	default:
	    return ('X');
    }
}


int Wr_P_line_asc ( int  num , struct P_line *ptr, FILE *fp)
{
    fprintf( fp, "%4d  N1: %4d,  N2: %d  Right: %4d  Left: %4d\n", 
	num,  ptr->N1, ptr->N2, ptr->right, ptr->left) ;
    fprintf( fp, "      N: %10.2f,  S: %10.2f,  E: %10.2f,   W: %10.2f\n",
	ptr->N, ptr->S, ptr->E, ptr->W) ;

    fprintf( fp, "      offset: %ld,  att: %d,  type: %c\n",
	ptr->offset, ptr->att, codes(ptr->type)) ;

    return (0);
}


int Wr_P_area_asc ( int num, struct P_area *ptr, FILE *fp)
{
    register int i, j;

    fprintf (fp, "%4d   Att %d  N_lines %d\n", num, ptr->att, ptr->n_lines);
    fprintf( fp, "  N: %10.2f,  S: %10.2f,  E: %10.2f,   W: %10.2f\n",
	ptr->N, ptr->S, ptr->E, ptr->W) ;
    
    for (i = 0 ; i < ptr->n_lines ; i+=8)
    {
	fprintf (fp, "   ");
	for (j = i ; j < i+8 && j < ptr->n_lines ; j++)
	    fprintf (fp, " %d", ptr->lines[j]);
	fprintf (fp, "\n");
    }
    return (0);
}

int Wr_P_att_asc ( int num, struct P_att *ptr, FILE *fp)
{
    fprintf( fp, "%4d  %c x: %10.2f,  y: %10.2f  Cat: %6d  ID: %6d\n", 
	num, codes(ptr->type), ptr->x, ptr->y, ptr->cat, ptr->index) ;

    return(0) ;
}


int Wr_P_isle_asc ( int num, struct P_isle *ptr, FILE *fp)
{
    register int i, j;

    fprintf (fp, "%4d   N_lines %d\n", num, ptr->n_lines);
    fprintf( fp, "  N: %10.2f,  S: %10.2f,  E: %10.2f,   W: %10.2f\n",
	ptr->N, ptr->S, ptr->E, ptr->W) ;
    
    for (i = 0 ; i < ptr->n_lines ; i+=8)
    {
	fprintf (fp, "   ");
	for (j = i ; j < i+8 && j < ptr->n_lines ; j++)
	    fprintf (fp, " %d", ptr->lines[j]);
	fprintf (fp, "\n");
    }
    return (0);
}

int Wr_Plus_head_asc ( struct Plus_head *ptr, FILE *fp)
{

    rewind(fp) ;
    
    fprintf( fp, "n_nodes: %d,  n_lines: %d,  n_areas: %d\n",
	ptr->n_nodes, ptr->n_lines, ptr->n_areas) ;
    fprintf( fp, "n_llines: %d,  n_alines: %d,  n_points: %d\n",
	ptr->n_llines, ptr->n_alines, ptr->n_points) ;

    fprintf( fp, "Node_offset: %ld,         Line_offset: %ld,\n  Area_offset: %ld,       Att_offset: %ld\n",
	ptr->Node_offset, ptr->Line_offset, ptr->Area_offset, ptr->Att_offset) ;

    fprintf( fp, "DIG_SIZE: %ld  DIG_CODE: %ld\n", ptr->Dig_size,ptr->Dig_code);
    fprintf( fp, "ATT_SIZE: %ld  ATT_CODE: %ld\n", ptr->Att_size,ptr->Att_code);

    fprintf( fp, "Version Major: %d, Minor: %d\n",ptr->Major, ptr->Minor);
    fprintf( fp, "Back_Major: %ld,  Back_Minor: %ld,  future3: %ld,  future4: %ld\n",
	ptr->Back_Major, ptr->Back_Minor, ptr->future3, ptr->future4) ;

    fprintf( fp, "F1: %f,  F2: %f,  F3: %f,  F4: %f\n",
	ptr->F1, ptr->F2, ptr->F3, ptr->F4) ;
    
    fprintf( fp, "Dig_name: '%s'\n", ptr->Dig_name) ;

    fprintf( fp, "filler: '%s'\n", ptr->filler) ;

    return (0);
}
