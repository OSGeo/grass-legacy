
#ifndef GRASS_SPAWN_H
#define GRASS_SPAWN_H

#define SF_REDIRECT_FILE		((char *) 1)
#define SF_REDIRECT_DESCRIPTOR		((char *) 2)
#define SF_CLOSE_DESCRIPTOR		((char *) 3)
#define SF_SIGNAL			((char *) 4)
#define SF_VARIABLE			((char *) 5)
#define SF_BINDING			((char *) 6)
#define SF_BACKGROUND			((char *) 7)
#define SF_DIRECTORY			((char *) 8)

enum signal_action
{
	SSA_NONE,
	SSA_IGNORE,
	SSA_DEFAULT,
	SSA_BLOCK,
	SSA_UNBLOCK,
};

enum signal_type
{
	SST_PRE,
	SST_POST,
	SST_CHILD,
};

extern int G_spawn(char *command, ...);
extern int G_spawn_ex(char *command, ...);

#endif

