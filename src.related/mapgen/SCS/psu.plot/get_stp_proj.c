/* %W% %G% */
/* get_stp_proj.c    1.0   3/13/91
*    Created by : R.L.Glenn , Soil Conservation Service, USDA
*    Purpose: function
*	      Provides a means of getting desired projection
*             parameters for st.plane projections
*    Input arguements : empty string
*    Output arguements: projection parameters in string
*           
*/

#include <stdio.h>
#include <ctype.h>

static char *st_abbr[] = {
"AL", "AK", "AS", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI",
"ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN",
"MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH",
"OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA",
"WV", "WI", "WY", "PR", "VI", "GU", NULL
};
static char *st_codes[] = {
"ALe: Alabama east", "ALw: Alabama west",
"AK1: Alaska zone no. 1", "AK2: Alaska zone no. 2", "AK3: Alaska zone no. 3",
"AK4: Alaska zone no. 4", "AK5: Alaska zone no. 5", "AK6: Alaska zone no. 6",
"AK7: Alaska zone no. 7", "AK8: Alaska zone no. 8", "AK9: Alaska zone no. 9",
"AK10: Alaska zone no. 10",
"AZe: Arizona east", "AZc: Arizona central", "AZw: Arizona west",
"ARn: Arkansas north", "ARs: Arkansas south",
"CA1: California i", "CA2: California ii", "CA3: California iii",
"CA4: California iv", "CA5: California v", "CA6: California vi",
"CA7: California vii",
"COn: Colorado north", "COc: Colorado central", "COs: Colorado south",
"CT: Connecticut ",
"DE: Delaware ",
"FLe: Florida east", "FLw: Florida west", "FLn: Florida north",
"GAe: Georgia east", "GAw: Georgia west",
"HI1: Hawaii 1", "HI2: Hawaii 2", "HI3: Hawaii 3",
"HI4: Hawaii 4", "HI5: Hawaii 5",
"IDe: Idaho east", "IDc: Idaho central", "IDw: Idaho west",
"ILe: Illinois east", "ILw: Illinois west",
"INe: Indiana east", "INw: Indiana west",
"IAn: Iowa north", "IAs: Iowa south",
"KSn: Kansas north", "KSs: Kansas south",
"KYn: Kentucky north", "KYs: Kentucky south",
"LAn: Louisiana north", "LAs: Louisiana south", "LAos: Louisiana offshore",
"MEe: Maine east", "MEw: Maine west",
"MD: Maryland ",
"MAm: Massachusetts mainland", "MAi: Massachusetts island",
"MIe: Michigan east", "MIcm: Michigan central/m", "MIw: Michigan west",
"MIn: Michigan north", "MIcl: Michigan central/l", "MIs: Michigan south",
"MNn: Minnesota north", "MNc: Minnesota central", "MNs: Minnesota south",
"MSe: Mississippi east", "MSw: Mississippi west",
"MOe: Missouri east", "MOc: Missouri central", "MOw: Missouri west",
"MTn: Montana north", "MTc: Montana central", "MTs: Montana south",
"NEn: Nebraska north", "NEs: Nebraska south",
"NVe: Nevada east", "NVc: Nevada central", "NVw: Nevada west",
"NH: New Hampshire ",
"NJ: New Jersey ",
"NMe: New Mexico east", "NMc: New Mexico central", "NMw: New Mexico west",
"NYe: New York east", "NYc: New York central", "NYw: New York west",
"NYli: New York Long Island",
"NC: North Carolina ",
"NDn: North Dakota north", "NDs: North Dakota south",
"OHn: Ohio north", "OHs: Ohio south",
"OKn: Oklahoma north", "OKs: Oklahoma south",
"ORn: Oregon north", "ORs: Oregon south",
"PAn: Pennsylvania north", "PAs: Pennsylvania south",
"RI: Rhode Island ",
"SCn: South Carolina north", "SCs: South Carolina south",
"SDn: South Dakota north", "SDs: South Dakota south",
"TN: Tennessee ",
"TXn: Texas north", "TXnc: Texas north central", "TXc: Texas central",
"TXsc: Texas south central", "TXs: Texas south",
"UTn: Utah north", "UTc: Utah central", "UTs: Utah south",
"VT: Vermont ",
"VAn: Virginia north", "VAs: Virginia south",
"WAn: Washington north", "WAs: Washington south",
"WVn: West Virginia north", "WVs: West Virginia south",
"WIn: Wisconsin north", "WIc: Wisconsin central", "WIs: Wisconsin south",
"WYe: Wyoming east", "WYec: Wyoming east central",
"WYwc: Wyoming west central", "WYw: Wyoming west",
"PR: Puerto Rico",
"VI: Virgin Islands",
"VIsc: St. Croix",
"GU: Guam",
"AS: Samoa", NULL
};
static char *st_proj[] = {
"+proj=tmerc\t+k=0.99996\t+x_0=152400.3048\t+lon_0=85d50'w +lat_0=30d30'n",
"+proj=tmerc\t+k=0.999933333333\t+x_0=152400.3048\t+lon_0=87d30'w +lat_0=30n",
"+proj=omerc\t+azi\t+k=.9999\t+lonc=133d40'w\t+alpha=-36.869897645844\t+lat_0=57dn\t+x_0=5000000\t+y_0=-5000000",
"+proj=tmerc\t+k=0.9999\t+x_0=152400.3048\t+lon_0=142w\t+lat_0=54n",
"+proj=tmerc\t+k=0.9999\t+x_0=152400.3048\t+lon_0=146w\t+lat_0=54n",
"+proj=tmerc\t+k=0.9999\t+x_0=152400.3048\t+lon_0=150w\t+lat_0=54n",
"+proj=tmerc\t+k=0.9999\t+x_0=152400.3048\t+lon_0=154w\t+lat_0=54n",
"+proj=tmerc\t+k=0.9999\t+x_0=152400.3048\t+lon_0=158w\t+lat_0=54n",
"+proj=tmerc\t+k=0.9999\t+x_0=213360.4267\t+lon_0=162w\t+lat_0=54n",
"+proj=tmerc\t+k=0.9999\t+x_0=152400.3048\t+lon_0=166w\t+lat_0=54n",
"+proj=tmerc\t+k=0.9999\t+x_0=182880.3658\t+lon_0=170w\t+lat_0=54n",
"+proj=lcc\t+lat_1=51d50'n +lat_2=53d50'n\t+lon_0=176w\t+lat_0=51n\t+x_0=914401.8288",
"+proj=tmerc\t+k=0.9999\t+x_0=152400.3048\t+lon_0=110d10'w +lat_0=31n",
"+proj=tmerc\t+k=0.9999\t+x_0=152400.3048\t+lon_0=111d55'w +lat_0=31n",
"+proj=tmerc\t+k=0.999933333333\t+x_0=152400.3048\t+lon_0=113d45'w +lat_0=31n",
"+proj=lcc\t+lat_1=34d56'n +lat_2=36d14'n\t+lon_0=92w\t+lat_0=34d20'n +x_0=609601.2192",
"+proj=lcc\t+lat_1=33d18'n +lat_2=34d46'n\t+lon_0=92w\t+lat_0=32d40'n +x_0=609601.2192",
"+proj=lcc\t+lat_1=40n\t+lat_2=41d40'n +lon_0=122w +lat_0=39d20'n\t+x_0=609601.2192",
"+proj=lcc\t+lat_1=38d20'n +lat_2=39d50'n\t+lon_0=122w\t+lat_0=37d40'n +x_0=609601.2192",
"+proj=lcc\t+lat_1=37d4'n +lat_2=38d26'n\t+lon_0=120d30'w +lat_0=36d30'n\t+x_0=609601.2192",
"+proj=lcc\t+lat_1=36n\t+lat_2=37d15'n +lon_0=119w +lat_0=35d20'n\t+x_0=609601.2192",
"+proj=lcc\t+lat_1=34d2'n +lat_2=35d28'n\t+lon_0=118w\t+lat_0=33d30'n +x_0=609601.2192",
"+proj=lcc\t+lat_1=32d47'n +lat_2=33d53'n\t+lon_0=116d15'w +lat_0=32d10'n\t+x_0=609601.2192",
"+proj=lcc\t+lat_1=33d52'n +lat_2=34d25'n\t+lon_0=118d20'w +lat_0=34d8'n\t+x_0=1276106.4506\t+y_0=1268253.0069",
"+proj=lcc\t+lat_1=39d43'n +lat_2=40d47'n\t+lon_0=105d30'w +lat_0=39d20'n\t+x_0=609601.2192",
"+proj=lcc\t+lat_1=38d27'n +lat_2=39d45'n\t+lon_0=105d30'w +lat_0=37d50'n\t+x_0=609601.2192",
"+proj=lcc\t+lat_1=37d14'n +lat_2=38d26'n\t+lon_0=105d30'w +lat_0=36d40'n\t+x_0=609601.2192",
"+proj=lcc\t+lat_1=41d12'n +lat_2=41d52'n\t+lon_0=72d45'w +lat_0=40d50'n\t+x_0=182880.3658",
"+proj=tmerc\t+k=0.999995\t+x_0=152400.3048\t+lon_0=75d25'w +lat_0=38n",
"+proj=tmerc\t+k=0.999941176471\t+x_0=152400.3048\t+lon_0=81w\t+lat_0=24d20'n",
"+proj=tmerc\t+k=0.999941176471\t+x_0=152400.3048\t+lon_0=82w\t+lat_0=24d20'n",
"+proj=lcc\t+lat_1=29d35'n +lat_2=30d45'n\t+lon_0=84d30'w +lat_0=29n +x_0=609601.2192",
"+proj=tmerc\t+k=0.9999\t+x_0=152400.3048\t+lon_0=82d10'w +lat_0=30n",
"+proj=tmerc\t+k=0.9999\t+x_0=152400.3048\t+lon_0=84d10'w +lat_0=30n",
"+proj=tmerc\t+k=0.999966666667\t+x_0=152400.3048\t+lon_0=155d30'w +lat_0=18d50'n",
"+proj=tmerc\t+k=0.999966666667\t+x_0=152400.3048\t+lon_0=156d40'w +lat_0=20d20'n",
"+proj=tmerc\t+k=0.99999\t+x_0=152400.3048\t+lon_0=158w\t+lat_0=21d10'n",
"+proj=tmerc\t+k=0.99999\t+x_0=152400.3048\t+lon_0=159d30'w +lat_0=21d50'n",
"+proj=tmerc\t+k=1.\t+x_0=152400.3048\t+lon_0=160d10'w +lat_0=21d40'n",
"+proj=tmerc\t+k=0.999947368421\t+x_0=152400.3048\t+lon_0=112d10'w +lat_0=41d40'n",
"+proj=tmerc\t+k=0.999947368421\t+x_0=152400.3048\t+lon_0=114w\t+lat_0=41d40'n",
"+proj=tmerc\t+k=0.999933333333\t+x_0=152400.3048\t+lon_0=115d45'w +lat_0=41d40'n",
"+proj=tmerc\t+k=0.999975\t+x_0=152400.3048\t+lon_0=88d20'w +lat_0=36d40'n",
"+proj=tmerc\t+k=0.999941176471\t+x_0=152400.3048\t+lon_0=90d10'w +lat_0=36d40'n",
"+proj=tmerc\t+k=0.999966666667\t+x_0=152400.3048\t+lon_0=85d40'w +lat_0=37d30'n",
"+proj=tmerc\t+k=0.999966666667\t+x_0=152400.3048\t+lon_0=87d5'w +lat_0=37d30'n",
"+proj=lcc\t+lat_1=42d4'n +lat_2=43d16'n\t+lon_0=93d30'w +lat_0=41d30'n\t+x_0=609601.2192",
"+proj=lcc\t+lat_1=40d37'n +lat_2=41d47'n\t+lon_0=93d30'w +lat_0=40n +x_0=609601.2192",
"+proj=lcc\t+lat_1=38d43'n +lat_2=39d47'n\t+lon_0=98w\t+lat_0=38d20'n +x_0=609601.2192",
"+proj=lcc\t+lat_1=37d16'n +lat_2=38d34'n\t+lon_0=98d30'w +lat_0=36d40'n\t+x_0=609601.2192",
"+proj=lcc\t+lat_1=37d58'n +lat_2=38d58'n\t+lon_0=84d15'w +lat_0=37d30'n\t+x_0=609601.2192",
"+proj=lcc\t+lat_1=36d44'n +lat_2=37d56'n\t+lon_0=85d45'w +lat_0=36d20'n\t+x_0=609601.2192",
"+proj=lcc\t+lat_1=31d10'n +lat_2=32d40'n\t+lon_0=92d30'w +lat_0=30d40'n\t+x_0=609601.2192",
"+proj=lcc\t+lat_1=29d18'n +lat_2=30d42'n\t+lon_0=91d20'w +lat_0=28d40'n\t+x_0=609601.2192",
"+proj=lcc\t+lat_1=26d10'n +lat_2=27d50'n\t+lon_0=91d20'w +lat_0=25d40'n\t+x_0=609601.2192",
"+proj=tmerc\t+k=0.9999\t+x_0=152400.3048\t+lon_0=68d30'w +lat_0=43d50'n",
"+proj=tmerc\t+k=0.999966666667\t+x_0=152400.3048\t+lon_0=70d10'w +lat_0=42d50'n",
"+proj=lcc\t+lat_1=38d18'n +lat_2=39d27'n\t+lon_0=77w\t+lat_0=37d50'n +x_0=243840.4877",
"+proj=lcc\t+lat_1=41d43'n +lat_2=42d41'n\t+lon_0=71d30'w +lat_0=41n +x_0=182880.3658",
"+proj=lcc\t+lat_1=41d17'n +lat_2=41d29'n\t+lon_0=70d30'w +lat_0=41n +x_0=60960.1219",
"+proj=tmerc\t+k=0.999942857143\t+x_0=152400.3048\t+lon_0=83d40'w +lat_0=41d30'n",
"+proj=tmerc\t+k=0.999909090909\t+x_0=152400.3048\t+lon_0=85d45'w +lat_0=41d30'n",
"+proj=tmerc\t+k=0.999909090909\t+x_0=152400.3048\t+lon_0=88d45'w +lat_0=41d30'n",
"+proj=lcc\t+lat_1=45d29'n +lat_2=47d5'n\t+lon_0=88d45'w +lat_0=44d47'n\t+x_0=609601.2192",
"+proj=lcc\t+lat_1=44d11'n +lat_2=45d42'n\t+lon_0=88d45'w +lat_0=43d19'n\t+x_0=609601.2192",
"+proj=lcc\t+lat_1=42d6'n +lat_2=43d40'n\t+lon_0=88d45'w +lat_0=41d30'n\t+x_0=609601.2192",
"+proj=lcc\t+lat_1=47d2'n +lat_2=48d38'n\t+lon_0=93d6'w +lat_0=46d30'n\t+x_0=609601.2192",
"+proj=lcc\t+lat_1=45d37'n +lat_2=47d3'n\t+lon_0=94d15'w +lat_0=45n +x_0=609601.2192",
"+proj=lcc\t+lat_1=43d47'n +lat_2=45d13'n\t+lon_0=94w\t+lat_0=43n\t+x_0=609601.2192",
"+proj=tmerc\t+k=0.99996\t+x_0=152400.3048\t+lon_0=88d50'w +lat_0=29d40'n",
"+proj=tmerc\t+k=0.999941176471\t+x_0=152400.3048\t+lon_0=90d20'w +lat_0=30d30'n",
"+proj=tmerc\t+k=0.999933333333\t+x_0=152400.3048\t+lon_0=90d30'w +lat_0=35d50'n",
"+proj=tmerc\t+k=0.999933333333\t+x_0=152400.3048\t+lon_0=92d30'w +lat_0=35d50'n",
"+proj=tmerc\t+k=0.999941176471\t+x_0=152400.3048\t+lon_0=94d30'w +lat_0=36d10'n",
"+proj=lcc\t+lat_1=47d51'n +lat_2=48d43'n\t+lon_0=109d30'w +lat_0=47n +x_0=609601.2192",
"+proj=lcc\t+lat_1=46d27'n +lat_2=47d53'n\t+lon_0=109d30'w +lat_0=45d50'n\t+x_0=609601.2192",
"+proj=lcc\t+lat_1=44d52'n +lat_2=46d24'n\t+lon_0=109d30'w +lat_0=44n +x_0=609601.2192",
"+proj=lcc\t+lat_1=41d51'n +lat_2=42d49'n\t+lon_0=100w\t+lat_0=41d20'n +x_0=609601.2192",
"+proj=lcc\t+lat_1=40d17'n +lat_2=41d43'n\t+lon_0=99d30'w +lat_0=39d40'n\t+x_0=609601.2192",
"+proj=tmerc\t+k=0.9999\t+x_0=152400.3048\t+lon_0=115d35'w +lat_0=34d45'n",
"+proj=tmerc\t+k=0.9999\t+x_0=152400.3048\t+lon_0=116d40'w +lat_0=34d45'n",
"+proj=tmerc\t+k=0.9999\t+x_0=152400.3048\t+lon_0=118d35'w +lat_0=34d45'n",
"+proj=tmerc\t+k=0.999966666667\t+x_0=152400.3048\t+lon_0=71d40'w +lat_0=42d30'n",
"+proj=tmerc\t+k=0.999975\t+x_0=609601.2192\t+lon_0=74d40'w +lat_0=38d50'n",
"+proj=tmerc\t+k=0.999909090909\t+x_0=152400.3048\t+lon_0=104d20'w +lat_0=31n",
"+proj=tmerc\t+k=0.9999\t+x_0=152400.3048\t+lon_0=106d15'w +lat_0=31n",
"+proj=tmerc\t+k=0.999916666667\t+x_0=152400.3048\t+lon_0=107d50'w +lat_0=31n",
"+proj=tmerc\t+k=0.999966666667\t+x_0=152400.3048\t+lon_0=74d20'w +lat_0=40n",
"+proj=tmerc\t+k=0.9999375\t+x_0=152400.3048\t+lon_0=76d35'w +lat_0=40n",
"+proj=tmerc\t+k=0.9999375\t+x_0=152400.3048\t+lon_0=78d35'w +lat_0=40n",
"+proj=lcc\t+lat_1=40d40'n +lat_2=41d2'n\t+lon_0=74w\t+lat_0=40d30'n +x_0=609601.2192 +y_0=30480.0610",
"+proj=lcc\t+lat_1=34d20'n +lat_2=36d10'n\t+lon_0=79w\t+lat_0=33d45'n +x_0=609601.2192",
"+proj=lcc\t+lat_1=47d26'n +lat_2=48d44'n\t+lon_0=100d30'w +lat_0=47n +x_0=609601.2192",
"+proj=lcc\t+lat_1=46d11'n +lat_2=47d29'n\t+lon_0=100d30'w +lat_0=45d40'n\t+x_0=609601.2192",
"+proj=lcc\t+lat_1=40d26'n +lat_2=41d42'n\t+lon_0=82d30'w +lat_0=39d40'n\t+x_0=609601.2192",
"+proj=lcc\t+lat_1=38d44'n +lat_2=40d2'n\t+lon_0=82d30'w +lat_0=38n +x_0=609601.2192",
"+proj=lcc\t+lat_1=35d34'n +lat_2=36d46'n\t+lon_0=98w\t+lat_0=35n\t+x_0=609601.2192",
"+proj=lcc\t+lat_1=33d56'n +lat_2=35d14'n\t+lon_0=98w\t+lat_0=33d20'n +x_0=609601.2192",
"+proj=lcc\t+lat_1=44d20'n +lat_2=46n +lon_0=120d30'w\t+lat_0=43d40'n +x_0=609601.2192",
"+proj=lcc\t+lat_1=42d20'n +lat_2=44n +lon_0=120d30'w\t+lat_0=41d40'n +x_0=609601.2192",
"+proj=lcc\t+lat_1=40d53'n +lat_2=41d57'n\t+lon_0=77d45'w +lat_0=40d10'n\t+x_0=609601.2192",
"+proj=lcc\t+lat_1=39d56'n +lat_2=40d58'n\t+lon_0=77d45'w +lat_0=39d20'n\t+x_0=609601.2192",
"+proj=tmerc\t+k=0.99999375\t+x_0=152400.3048\t+lon_0=71d30'w +lat_0=41d5'n",
"+proj=lcc\t+lat_1=33d46'n +lat_2=34d58'n\t+lon_0=81w\t+lat_0=33n\t+x_0=609601.2192",
"+proj=lcc\t+lat_1=32d20'n +lat_2=33d40'n\t+lon_0=81w\t+lat_0=31d50'n +x_0=609601.2192",
"+proj=lcc\t+lat_1=44d25'n +lat_2=45d41'n\t+lon_0=100w\t+lat_0=43d50'n +x_0=609601.2192",
"+proj=lcc\t+lat_1=42d50'n +lat_2=44d24'n\t+lon_0=100d20'w +lat_0=42d20'n\t+x_0=609601.2192",
"+proj=lcc\t+lat_1=35d15'n +lat_2=36d25'n\t+lon_0=86w\t+lat_0=34d40'n +x_0=609601.2192 +y_0=30480.0610",
"+proj=lcc\t+lat_1=34d39'n +lat_2=36d11'n\t+lon_0=101d30'w +lat_0=34n +x_0=609601.2192",
"+proj=lcc\t+lat_1=32d8'n +lat_2=33d58'n\t+lon_0=97d30'w +lat_0=31d40'n\t+x_0=609601.2192",
"+proj=lcc\t+lat_1=30d7'n +lat_2=31d53'n\t+lon_0=100d20'w +lat_0=29d40'n\t+x_0=609601.2192",
"+proj=lcc\t+lat_1=28d23'n +lat_2=30d17'n\t+lon_0=99w\t+lat_0=27d50'n +x_0=609601.2192",
"+proj=lcc\t+lat_1=26d10'n +lat_2=27d50'n\t+lon_0=98d30'w\t+lat_0=25d40'n\t+x_0=609601.2192",
"+proj=lcc\t+lat_1=40d43'n +lat_2=41d47'n\t+lon_0=111d30'w +lat_0=40d20'n\t+x_0=609601.2192",
"+proj=lcc\t+lat_1=39d1'n +lat_2=40d39'n\t+lon_0=111d30'w +lat_0=38d20'n\t+x_0=609601.2192",
"+proj=lcc\t+lat_1=37d13'n +lat_2=38d21'n\t+lon_0=111d30'w +lat_0=36d40'n\t+x_0=609601.2192",
"+proj=tmerc\t+k=0.999964285714\t+x_0=152400.3048\t+lon_0=72d30'w +lat_0=42d30'n",
"+proj=lcc\t+lat_1=38d2'n +lat_2=39d12'n\t+lon_0=78d30'w +lat_0=37d40'n\t+x_0=609601.2192",
"+proj=lcc\t+lat_1=36d46'n +lat_2=37d58'n\t+lon_0=78d30'w +lat_0=36d20'n\t+x_0=609601.2192",
"+proj=lcc\t+lat_1=47d30'n +lat_2=48d44'n\t+lon_0=120d50'w +lat_0=47n +x_0=609601.2192",
"+proj=lcc\t+lat_1=45d50'n +lat_2=47d20'n\t+lon_0=120d30'w +lat_0=45d20'n\t+x_0=609601.2192",
"+proj=lcc\t+lat_1=39n\t+lat_2=40d15'n +lon_0=79d30'w\t+lat_0=38d30'n +x_0=609601.2192",
"+proj=lcc\t+lat_1=37d29'n +lat_2=38d53'n\t+lon_0=81w\t+lat_0=37n\t+x_0=609601.2192",
"+proj=lcc\t+lat_1=45d34'n +lat_2=46d46'n\t+lon_0=90w\t+lat_0=45d10'n +x_0=609601.2192",
"+proj=lcc\t+lat_1=44d15'n +lat_2=45d30'n\t+lon_0=90w\t+lat_0=43d50'n +x_0=609601.2192",
"+proj=lcc\t+lat_1=42d44'n +lat_2=44d4'n\t+lon_0=90w\t+lat_0=42n\t+x_0=609601.2192",
"+proj=tmerc\t+k=0.999941176471\t+x_0=152400.3048\t+lon_0=105d10'w +lat_0=40d40'n",
"+proj=tmerc\t+k=0.999941176471\t+x_0=152400.3048\t+lon_0=107d20'w +lat_0=40d40'n",
"+proj=tmerc\t+k=0.999941176471\t+x_0=152400.3048\t+lon_0=108d45'w +lat_0=40d40'n",
"+proj=tmerc\t+k=0.999941176471\t+x_0=152400.3048\t+lon_0=110d5'w +lat_0=40d40'n",
"+proj=lcc\t+lat_1=18d2'n +lat_2=18d26'n\t+lon_0=66d26'w +lat_0=17d50'n\t+x_0=152400.3048",
"+proj=lcc\t+lat_1=18d2'n +lat_2=18d26'n\t+lon_0=66d26'w +lat_0=17d50'n\t+x_0=152400.3048\t+y_0=30480.0610",
"+proj=lcc\t+lat_1=18d2'n +lat_2=18d26'n\t+lon_0=66d26'w +lat_0=17d50'n\t+x_0=152400.3048\t+y_0=30480.0610",
"+proj=poly\t+lon_0=144d44'55.50254\"e\t+lat_0=13d28'20.87887\"n\t+x_0=500000\t+y_0=500000",
"+proj=lcc\t+lat_1=14d16's +lat_2=14d16's\t+lon_0=170w\t+lat_0=14d16's +x_0=152400.3048 +y_0=95169.3117", NULL
};

get_stp_proj(string)
char *string;
{
    int i, cnt, found, code, last_cnt;
    char answer[10], buf[100], *p;

    found = 0;
    while(!found)
       {
       fprintf(stderr,"\tEnter State abbreviation : ");
       gets(answer);
       if (strlen(answer) == 0) 
	   {
	   sprintf(buf,"State abbrevation is required \n") ;
	   G_fatal_error(buf) ;
	   return(0);
           }
       else 
	   {             /* check for valid abbrevation */
	   for (cnt=0;st_abbr[cnt] != NULL;cnt++)
	     if (strcmp(answer,st_abbr[cnt]) == 0)
		{
		found = 1;
		break;
		}
           if (! found)
	      {
              fprintf(stderr,"State abbrevation NOT found\n");
	      fprintf(stderr,"\nKnown abbrevations:\n\n");
              i = 0;
	      for (cnt=0;st_abbr[cnt] != NULL;cnt++)
                 {
	         fprintf (stderr,"%s  ", st_abbr[cnt]);
                 i++;
                 if (i == 10) 
                     {
                     fprintf(stderr,"\n");
                     i = 0;
                     }
	         }
              if (i > 0) fprintf(stderr,"\n");
	      }
	   }
       }
    
    found = 0;
    while (! found)
       {
       code = 1;
       for (cnt=0; st_codes[cnt] != NULL; cnt++)
	  {
	  if (strncmp(answer, st_codes[cnt], 2) == 0)
             {
	     fprintf (stderr,"\n\t%d. %s",code++,st_codes[cnt]);
             last_cnt = cnt;
             }
          }
       code --;
       fprintf(stderr,"\n\t\tSelect which zone : ");
       gets(buf);
       if (strlen(buf) == 0) 
	   {
	   sprintf(buf,"Zone selection is required \n") ;
	   G_fatal_error(buf) ;
	   return(0);
           }
       else
	  {
	  if ( *buf == '1' || *buf == '2' || *buf == '3' ||
	       *buf == '4' || *buf == '5' || *buf == '6' ||
	       *buf == '7' || *buf == '8' || *buf == '9' ||
               *buf == '0' )
	       {
	       found = atoi(buf);
	       if (found > 0 && found <= code) 
		  {
	          cnt = last_cnt - code + found;
		  break;
		  }
               else found = 0;
	       }
          }
       fprintf(stderr," Invalid selection \n");
       sleep(2);
       }
    
    strcat(string,st_proj[cnt]);
	    /* get arg count */
    p = string;
    cnt = 0;
    while(strlen(p) > 0)
       {
       if (*p == '\t') cnt++;
       p++;
       }
    return (cnt+1);
}

main()
{
int n;
char string[100];

n = get_stp_proj(string);
printf("%s\n",string);
}
