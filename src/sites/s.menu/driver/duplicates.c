#include "site.h"
#include <signal.h>
#include <setjmp.h>

static jmp_buf jmp_env;

duplicates (s1)

	SITE_LIST *s1;
{
	SITE_LIST s2;
	char *desc1, *desc2;
	int n1, e1;
	int n2, e2;
	int dups;
	int same;
	int interrupted;
	int (*sigint)();
	int (*sigquit)();
	int catch();

	initialize_site_list (&s2);
	printf ("checking for duplicates\n");

	if (interrupted = setjmp (jmp_env))
		goto done;

	sigint = signal (SIGINT, catch);
	sigquit = signal (SIGQUIT, catch);

	dups = 0;

	strcpy (s2.name, s1->name);
	strcpy (s2.desc, s1->desc);

	rewind_site_list (s1);
	while (next_site (s1, &n1, &e1, &desc1))
	{
		rewind_site_list (&s2);
		for (same = 0; !same && next_site (&s2, &n2, &e2, &desc2);)
		{
			same = (n1 == n2 && e1 == e2);
			/*
			same = strcmp (desc1, desc2) == 0;
			*/
		}
		if (!same)
			add_site (&s2, n1, e1, desc1);
		else
		{
			if (!dups)
				printf("duplicates:\n");
			dups = 1;
			printf("\n");
			printf("%10d(E) %10d(N)    %s\n", e1,n1, desc1);
			printf("%10d(E) %10d(N)    %s\n", e2,n2, desc2);
		}
	}
	rewind_site_list (s1);
	rewind_site_list (&s2);

	if (dups)
	{
		printf("\n");
		if (yes("would you like them removed? "))
		{
			signal (SIGINT, SIG_IGN);
			signal (SIGQUIT, SIG_IGN);
			free_site_list (s1) ;
			copy_sites (&s2, s1, 0);
		}
	}
	else
	{
		printf("\nno duplicates\n");
	}

done:
	free_site_list( &s2 );
	signal (SIGQUIT, sigquit);
	signal (SIGINT, sigint);
	if (interrupted)
		printf("interrupted\n");

	return 1;
}
static catch(n)
{
	longjmp (jmp_env, n);
}
