#include <stdio.h>
#include "gis.h"
#include "Vect.h"
#include "dbmi.h"
#include "proto.h"

FILE *rulefd;
#define KEY(x) (strcmp(key,x)==0)

int 
main (int argc, char *argv[])
{
    struct Flag *d_flag;
    struct Option *input, *output, *table, *keycol, *rule, *typopt, *title;
    struct Categories cats;
    char *tit, *key, *data, buf[1024];
    int i, dissolve=0, rclelem, cat, ncat;
    unsigned int  type;
    char *old_name, *old_mapset, *new_name;
    dbRclsRule *rules;
    dbCatValI *rcl;


    G_gisinit (argv[0]);

    d_flag = G_define_flag();
    d_flag->key              = 'd';
    d_flag->description      = "Dissolve common boundaries (default is no) ";

    input = G_define_option();
    input->key = "input";
    input->required = YES;
    input->type = TYPE_STRING;
    input->gisprompt  = "old,dig,vector" ;
    input->description =  "Vector map to be reclassified";

    output = G_define_option();
    output->key = "output";
    output->required = YES;
    output->type = TYPE_STRING;
    output->gisprompt  = "new,dig,vector" ;
    output->description =  "Name for the resulting vector map";

    rule = G_define_option();
    rule->key = "rules";
    rule->required = YES;
    rule->type = TYPE_STRING;
    rule->description =  "Full path to the reclass rule file";
    
    table = G_define_option();
    table->key = "table";
    table->required = YES;
    table->type = TYPE_STRING;
    table->description =  "Table name";    

    keycol = G_define_option();
    keycol->key = "key";
    keycol->required = NO;
    keycol->type = TYPE_STRING;
    keycol->description =  "Key (id) column name";

    typopt = G_define_option();
    typopt->key              = "type";
    typopt->type             =  TYPE_STRING;
    typopt->required         =  YES;
    typopt->multiple         =  YES;
    typopt->options          =  "area,line,site";
    typopt->description      =  "Select area, line, or site ";

    title = G_define_option();
    title->key = "title";
    title->required = NO;
    title->type = TYPE_STRING;
    title->description =  "Title for the resulting vector map";

    if (G_parser(argc, argv))
	exit(1);
    if (d_flag->answer) dissolve = 1;
    old_name = input->answer;
    new_name = output->answer;
    tit    = title->answer;

    /* create dig directory, if not existing */
    G__make_mapset_element("dig_cats");

    old_mapset = G_find_file2 ("dig", old_name, "");
    if (old_mapset == NULL)
    {
	sprintf (buf, "%s - not found", old_name);
	G_fatal_error (buf);
	exit(1);
    }
    if (G_legal_filename(new_name) < 0)
    {
	sprintf (buf, "%s - illegal name", new_name);
	G_fatal_error (buf);
	exit(1);
    }
    if (strcmp(old_name,new_name)==0 && strcmp(old_mapset,G_mapset())== 0)
    {
	G_fatal_error ("input map can NOT be the same as output map");
	exit(1);
    }

    /* element types */
    i=0;
    type=0;
    while (typopt->answers[i])
    {
        if ( *typopt->answers[i] == 'l')  type |= LINE;
        else if ( *typopt->answers[i] == 'a')  type |= AREA;
        else if ( *typopt->answers[i] == 's')  type |= DOT;
	i++;
    }

    rules = db_rcls_rule_init(100);    
    rules->table = table->answer; 
    rules->key = keycol->answer;

    /* open rule file */
    if ((rulefd = fopen(rule->answer, "r")) == NULL)
    {
	sprintf (buf, "unable to open rule file %s", rule->answer);
	G_fatal_error (buf);
	exit(1);
    }


    G_init_cats (0, "", &cats);
    G_strcpy( buf, "No Data");
    G_set_cat(0, buf, &cats);
    if (tit == NULL)
	sprintf (tit=buf , "Reclass of %s in %s", old_name, old_mapset);
    G_set_cats_title (tit, &cats);

    while ( inpt(buf) )
    {
        if (!key_data(buf, &key, &data))
	    continue ;

        if (KEY("cat"))
	{	
            if (rules->count >= rules->alloc)
            {
		db_rcls_rule_extend(rules,  rules->alloc + 100);
            }    

	    G_strip(data);
	    cat=atoi(data);

	    if( cat <= 0 )
	    {
		sprintf (buf, "category %s invalid", data);
		G_fatal_error (buf);
		exit(1);
	    }
	    i= rules->count;
	    rules->cat[i] = cat;
	    rules->count++;
	    
	    continue;
        }

        if (KEY("label"))
	{	
	    G_strip(data);
	    i= rules->count-1;
	    rules->label[i] = G_store(data);
	    continue;
        }

        if (KEY("where"))
	{	
	    G_strip(data);
	    i= rules->count-1;
	    rules->where[i]=G_store(data);
	    continue;
        }
    }

    for (i = 0; i < rules->count; i++)
    {
	if( rules->where[i] == NULL )
	{
	    sprintf (buf, "where for category %d was not defined", rules->cat[i]);
	    G_fatal_error (buf);
	    exit(1);
	}

	if ( rules->label[i] == NULL)
	    rules->label[i] = rules->where[i];
	    
	G_set_cat ( rules->cat[i],  rules->label[i], &cats);
    }
    
    if (G_write_vector_cats (new_name, &cats) == -1)
    {
        sprintf (buf, "%s - unable to create category file", new_name);
        G_fatal_error (buf);
        exit(1);
    }
    G_free_cats (&cats); 

    /* create reclass table */
    fprintf(stderr, "\nSelecting from database.\n");
    if ( db_rcls (rules, &rcl, &ncat) != DB_OK )
    {
	fprintf(stderr," Error in creating reclass table\n");
	exit(1); 
    }

    /* reclass vector map */    
    rclelem = reclass ( old_name, new_name, rcl, ncat, type, dissolve);
    if ( 0 > rclelem)
    {
	fprintf(stderr," Error in area re-class processing\n");
	exit(1);
    }
    free (rcl);

    fprintf(stdout,"%d elments written to %s\n\n", rclelem, new_name);

    exit(0);
}
