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
