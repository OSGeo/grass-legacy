#include "gis.h"
/*****************************************************************
 *
 * G_yes (question, dflt)
 *
 * print the question and get a yes/no response from the user
 * if dflt is 1 a RETURN is taken to be yes
 * if dflt is 0 a RETURN is taken to be no
 * if dflt is -1 a RETURN is not a valid response
 *
 * returns 0 no, 1 yes
 ***************************************************************/
#include <stdio.h>


/*!
 * \brief ask a yes/no question
 *
 * This routine prints a <b>question</b> to the user, and expects the user to
 * respond either yes or no. (Invalid responses are rejected and the process is
 * repeated until the user answers yes or no.)
 * The <b>default</b> indicates what the RETURN key alone should mean. A
 * <b>default</b> of 1 indicates that RETURN means yes, 0 indicates that RETURN
 * means no, and -1 indicates that RETURN alone is not a valid response.
 * The <b>question</b> will be appended with ''(y/n) '', and, if
 * <b>default</b> is not -1, with ''[y] '' or ''[n] '', depending on the
 * <b>default.</b>
 * <i>G_yes</i> (~) returns 1 if the user said yes, and 0 if the user said no.
 *
 *  \param question
 *  \param default
 *  \return int
 */

int G_yes (char *question,int dflt)
{
    char answer[100];

    fflush (stdout);
    while (1)
    {
	fprintf (stderr,"%s", question);
	while (1)
	{
	    fprintf (stderr,"(y/n) ");
	    if (dflt >= 0) fprintf (stderr,dflt==0?"[n] ":"[y] ");
	    fflush (stderr);
	    if (!G_gets(answer)) break;
	    G_strip (answer);
	    switch (*answer)
	    {
	    case 'y': case 'Y':	return (1);
	    case 'n': case 'N':	return (0);
	    case 0: if (dflt >= 0) return (dflt);
	    }
	}
    }
}
