proc PQquery {dbname query} {
    PQsetdb $dbname
    set res [PQexec $query]
    set records ""
    set row ""
    if {[string range $res 0 0] == "E"} {
        puts stderr [format "%s\nretrieve_record failed" $res]
        return
    }
    if {[string range $res 0 0] != "P"} {
        puts stderr [format "retrieve_record: no portal?!?\n"]
        return
    }
    set portalname [string range $res 1 end]
    set hportal [PQparray $portalname]
    set ngroups [PQngroups $hportal]
    loop grpno 0 $ngroups 1 {
        set ntups [PQntuplesGroup $hportal $grpno]
        set nfields [PQnfieldsGroup $hportal $grpno]
	loop fldno 0 $nfields 1 {
                set fldname [PQfnameGroup $hportal $grpno $fldno]
		lappend row $fldname
		}
	lappend records $row
        loop tupno 0 $ntups 1 {
            set row ""
	    loop fldno 0 $nfields 1 { 
                set fldvalue [PQgetvalue $hportal $tupno $fldno]
                lappend row $fldvalue
            }
            lappend records $row
        }
    }
    PQfinish
    return $records
}

