/*
 * File: xg_history.h
 *
 * Desc: contains defs for history mechanism
 *
 * Auth: Kurt Buehler
 *
 * Date:
 *
 * Modification History:
 *
 *
 */
#ifndef XG_HISTORY_H
#define XG_HISTORY_H

#define XG_HISTORY_DISABLE	0
#define XG_HISTORY_ENABLE	1

#define XG_HISTORY_NONE		0
#define XG_HISTORY_EXEC_CAPTURE	1
#define XG_HISTORY_EXEC		2
#define XG_HISTORY_XCLIP	3

typedef struct _xg_history_item {
    int type;
    char *text;
    struct _xg_history_item *next;
} XgHistoryItemRec;

#endif /* XG_HISTORY_H */
