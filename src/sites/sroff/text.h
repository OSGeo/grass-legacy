#define TEXT struct _text
#define TEXT_LIST struct _textlist
TEXT
{
	char *buf;
	TEXT *next;
};
TEXT_LIST
{
	TEXT *first;
	TEXT *last;
	TEXT *cur;
};
/* text.c */
int add_text(TEXT_LIST *, char *);
int initialize_text_list(TEXT_LIST *);
int rewind_text_list(TEXT_LIST *);
int next_text(TEXT_LIST *, char **);
int free_text_list(TEXT_LIST *);
int count_text(TEXT_LIST *);
int output_text(TEXT_LIST *);
