interface_build {
    {r.profile} 0
    {Outputs the raster map layer values lying on user-defined line(s).}
    {entry map {Raster map to be queried:} 0 raster}
    {entry line {Profile coordinates (east,north,east,north[,...]):} 0 +xy}
    {entry width {Profile width, in cells (odd number) [1]:} 0 ""}
    {checkbox result {Raw results (default).} "" raw}
    {checkbox result {Median result (a single value).} "" median}
    {checkbox result {Average result (a single value).} "" average}
}
