struct signalflag
{
	int interrupt;
};
#ifdef MAIN
struct signalflag signalflag;
#else
extern struct signalflag signalflag;
#endif
