
#include "gis.h"

struct list_struct {
	char *string;
	struct list_struct *ptr;
};


/* globals !!! */
int Sheight, Swidth;
char Scurwin[100];

int Wtop, Wbot, Wleft, Wright;
char Wcell[100]="", Wcolor[25]="";

int Mtype, Mzone, Mrows, Mcols;
double Mewres, Mnsres, Mnorth, Msouth, Meast, Mwest;

struct list_struct *List = NULL, *List_last = NULL;

main(argc, argv)
int argc;
char **argv;
{
	char **pads;
	char **items;
	int npads;
	int nitems;
	int p;
	int stat ;
	struct list_struct *temp_list;
	struct Flag *all_flag;
	struct Flag *cur_frame;
	struct Option *opt1;
	char buff[1024];
	char current_frame[64] ;

	G_gisinit(argv[0]);

	R_open_driver();
	Sheight = R_screen_bot() - R_screen_top() + 1;
	Swidth = R_screen_rite() - R_screen_left() + 1;

	R_pad_list (&pads, &npads);

	/* process the screen pad */
	p = -1;
	stat = R_pad_select ("");
	if (stat) {
		R_pad_perror ("echo     ERROR", stat);
		printf("exit -1\n\n");
	}
	else process_pad(&items, &nitems);

	opt1 = G_define_option();
	opt1->key = "frame";
	opt1->description = "Name of frame(s) to save";
	opt1->type = TYPE_STRING;
	opt1->required = NO;
	opt1->answer = Scurwin;
	opt1->multiple = YES;
	buff[0] = '\0';
	for (p=npads-1; p>=0; p--) {
		strcat(buff, pads[p]);
		if (p != 0)  strcat(buff, ",");
	}
	opt1->options = buff;

	cur_frame = G_define_flag();
	cur_frame->key = 'c';
	cur_frame->description = "Save current frame";
	cur_frame->answer = 0;

	all_flag = G_define_flag();
	all_flag->key = 'a';
	all_flag->description = "Save all the frames";
	all_flag->answer = 0;

	if (G_parser(argc, argv))
		exit(1);

	if (cur_frame->answer)
	{
		D_get_cur_wind(current_frame) ;
		opt1->answer = current_frame ;
	}

	printf ("#!/bin/sh\n#Shell Script created by d.save %s\n\n", G_date());

	/* now start at the end (the earliest made window) and process them */
	for (p = npads-1; p >= 0; p--) {
		if (all_flag->answer || in_frame_list(opt1, pads[p]))
		{
			if(! cur_frame->answer)
				printf ("\n#Here are the commands to create window: %s\n", pads[p]);
			stat = R_pad_select (pads[p]);
			if (stat) {
				R_pad_perror ("echo     ERROR", stat);
				printf("exit -1\n\n");
				continue;
			}
			if (process_pad(&items, &nitems) != 0) continue;

			Wtop = (100.0 * Wtop)/Sheight + 0.5;
			Wbot = (100.0 * Wbot)/Sheight + 0.5;
			Wleft = (100.0 * Wleft)/Swidth + 0.5;
			Wright = (100.0 * Wright)/Swidth + 0.5;
			if (Wtop<0) Wtop=0;
			if (Wbot<0) Wbot=0;
			if (Wleft<0) Wleft=0;
			if (Wright<0) Wright=0;

			if(! cur_frame->answer)
			{
				if (all_flag->answer && p==npads-1)
					printf("d.frame -ec frame=%s at=%d,%d,%d,%d\n", pads[p],
						100-Wbot, 100-Wtop, Wleft, Wright);
				else
					printf("d.frame -c frame=%s at=%d,%d,%d,%d\n", pads[p],
						100-Wbot, 100-Wtop, Wleft, Wright);
			}

			if (Wcolor[0] == '\0')
				printf("d.erase\n");
			else
				printf("d.erase color=%s\n", Wcolor);

			if (Mtype != -1) {
				printf("g.region n=%lf s=%lf e=%lf w=%lf nsres=%lf ewres=%lf\n",
				    Mnorth, Msouth, Meast, Mwest, Mnsres, Mewres);
			}

			if (Wcell[0]!='\0')
				printf("d.rast map=%s\n", Wcell);

			/* print out the list */
			while (List!=NULL) {
				printf("%s\n", List->string);
				temp_list = List;
				List = List->ptr;
				free(temp_list->string);
				free(temp_list);
			}
			List_last = NULL;
			init_globals();
		}
		if (! all_flag->answer && ! strcmp(opt1->answer, pads[p])) break;
	}
	if (all_flag->answer || in_frame_list(opt1, Scurwin))
		printf("\nd.frame -s frame=%s\n", Scurwin);

	R_close_driver();
}

/* return 1 if the padname is in the opt->answers list of frame names */
in_frame_list(opt, padname)
struct Option *opt;
char *padname;
{
	int n;
	if (opt->answers)
		for (n=0; opt->answers[n] != NULL; n++)
			if (! strcmp(opt->answers[n], padname)) return(1);

	return(0);
}

init_globals()
{
	Wtop = Wbot = Wleft = Wright = 0;
	Wcell[0] = '\0';
	Wcolor[0] = '\0';

	Mtype = Mzone = -1;
	Mewres = Mnsres = Mnorth = Msouth = Meast = Mwest = 0.0;
}



/* this array of strings defines the possible item types */
#define ITEM_TYPES 7
#define ITEM_SIZE 9
char Known_items[ITEM_TYPES][ITEM_SIZE] = {
	"cur_w",
	"d_win",
	"m_win",
	"time",
	"cell",
	"list",
	"erase"
};


/* this function returns the position in the Known_items array that
   the given item string matches or -1 if no match. */
which_item(itemstr)
char *itemstr;
{
	int i;

	for(i=0; i<ITEM_TYPES; i++) {
		if (!strcmp(itemstr, Known_items[i])) return (i);
	}
	return (-1);
}

/* this function sets the global variable(s) associated with an item */
set_item(item, list)
char *item, **list;
{
	char tempbuf[100];

	if (!strcmp(item, "list")) process_list(item, list, 1);
	else {
		switch (which_item(item)) {
		case 0: /* cur_w */
			strcpy(Scurwin, list[0]);
			break;
		case 1:  /* d_win */
			sscanf(list[0]," %d %d %d %d ", &Wtop, &Wbot, &Wleft, &Wright);
			break;
		case 2:  /* m_win */
			sscanf(list[0], " %d %d %lf %lf %lf %lf %d %d ", &Mtype, &Mzone,
			    &Meast, &Mwest, &Mnorth, &Msouth, &Mrows, &Mcols);
			Mewres = (Meast - Mwest) / Mcols;
			Mnsres = (Mnorth - Msouth) / Mrows;
			break;
		case 3: /* time */
			break;
		case 4: /* cell */
			sscanf(list[0]," %s ", Wcell);
			break;
		case 6: /* d.erase color */
			sscanf(list[0]," %s ", Wcolor);
			break;
		default:
			sprintf(tempbuf,"Unkown item type in pad: %s", item);
			G_warning(tempbuf);
			break;
		}
	}
}

/* this function processes items which have multiple lines */
process_list(item, list, count)
int count;
char *item, **list;
{
	char tempbuf[100];
	int n;
	struct list_struct *new_list;

	switch (which_item(item)) {
	case 5: /* list */
		for (n = 0; n < count; n++) {
			new_list = (struct list_struct *) G_malloc(sizeof(struct list_struct));
			new_list->ptr = NULL;
			new_list->string = (char *) G_malloc(strlen(list[n])+1);
			strcpy(new_list->string, list[n]);
			if (List == NULL)  /* nothing on the list yet */
				List = new_list;
			else
				List_last->ptr = new_list;

			List_last = new_list;
		}
		break;
	default: /* otherwise */
		sprintf(tempbuf,"Unkown item type in pad: %s", item);
		G_warning(tempbuf);
		break;
	}

}


/* this function processes the items in a pad */
process_items(items, nitems)
int nitems;
char **items;
{
	int count;
	char **list;
	int i, n;
	int stat ;

	for (i = nitems-1; i >= 0; i--)
	{
		stat = R_pad_get_item (items[i], &list, &count);
		if (stat) {
			R_pad_perror ("#          ERROR", stat);
			printf("exit -1\n\n");
			continue;
		}
		if (count==1) set_item(items[i],list);
		else process_list(items[i],list, count);

		R_pad_freelist (list,count);
	}
}

/* this function processes a pad */
process_pad(items, nitems)
int *nitems;
char ***items;
{
	int stat;

	stat = R_pad_list_items (items, nitems);
	if (stat) {
		R_pad_perror ("echo     ERROR", stat);
		printf("exit -1\n\n");
		return(-1);
	}

	process_items(*items, *nitems);
	return(0);
}
