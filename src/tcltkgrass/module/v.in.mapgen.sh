interface_build {
    	{v.in.mapgen.sh} 0
    	{Imports MATLAB/mapgen vector files to GRASS vector map}
    	{entry input {Input MATLAB/mapgen vector file:} 0 file}
    	{entry name {Output vector map:} 0 vector}
	{checkbox f {Use MATLAB format for input.} -f}
	{checkbox s {Run v.support automatically on output vector map.} -s}
	{checkbox a {Also create ASCII vector file.} -a}
	{checkbox v {Run verbosely.} -v}
}
