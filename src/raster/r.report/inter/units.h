struct units
{
    char *name;
    char *code;
};
#ifndef GLOBAL
struct units units[] = {
	{"meters","me"},
	{"kilometers","k"},
	{"acres","a"},
	{"hectares","h"},
	{"miles","mi"},
	{"cell counts","c"},
	{"percent cover","p"},
        {NULL,NULL}};
#else
extern struct units units[];
#endif
