struct svrlabel
{
	char labeltype[60];
	int  nolabels;
};

struct catlbl
{
	int  catnum; 
	char label[60];
};
	
struct svlbl
{
	char labelname[60];
	char label[60];
};
	


struct svrlabel svrtmp[100];
struct svlbl	vtmp[100];
struct svlbl	stmp[100];
struct catlbl	cattmp[100];

char	reqramp[30];	
char	ramporientation[30];


int count;
int  tcats, nocats;
int  tvects, novects;
int  tsites, nosites;
int  isramp;
