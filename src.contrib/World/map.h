#define PI 3.14159265358979323844
#define RAD (PI/180)

struct coord {
	float l;	/* lat or lon in radians*/
	float s;	/* sin */
	float c;	/* cos */
};
struct place {
	struct coord nlat;
	struct coord wlon;
};
double sin(), cos(), tan(), abs(), sqrt(), log(), exp(), atan2();
