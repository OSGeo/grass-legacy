program:"m.region.ll"

title:"Convert Region UTM Coordinates to Geographic"

commandstring:"spheroid=(name)"

description:"m.region.ll converts Universal Transverse Mercator (UTM) coordinates falling within the current geographic region from UTM coordinates to geographic (latitude/longitude coordinates."

help:"This program allows you to convert Universal Transverse Mercator (UTM) coordinates falling within the current geographic region from UTM coordinates to geographic (latitude/longitude coordinates."
helpwidgetref:"06.export/06.05.coord:Map Coordinate and Projection Conversions"

{

    parameter name
	description:"Spheroid:"
	type:enum:"airy,australian,bessel,clark66,everest,grs80,hayford,international,krasovsky,wgs66,wgs72,wgs84"
	optional:false
	help:"Enter the desired spheroid."
	;
}
