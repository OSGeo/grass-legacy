#include <stdio.h>
#include <gl.h>
#include <device.h>
#include <panel.h>

static Panel *busy[30];

pnl_unselect_all (P)
    Panel *P;
{
    Panel *q;
    int i = 0;
    
    for (q=pnl_pl;q;q=q->next)
    {
	if (q == P)
	    continue;
	if (!q->selectable)
	    busy[i++] = q;
	else
	{
	    q->selectable = 0;
	    q->dirtycnt = 2;
	}
    }
    busy[i] = NULL;
}

pnl_select_all (P)
    Panel *P;
{
    Panel *q;
    int i;
    
    for (q=pnl_pl;q;q=q->next)
    {
	if (q == P)
	    continue;
	    
	for (i = 0 ; busy[i] != NULL ;i++)
	    if (busy[i] == q)
		break;

	if (busy[i] == NULL)
	{
	    q->selectable = 1;
	    q->dirtycnt = 2;
	}
    }
}
