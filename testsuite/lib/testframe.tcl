###############################################################################
# basic testing framework functions 
# only needed for testing without DejaGNU!
# source this file 
# c) Andreas Lange, andreas.lange@rhein-main.de
#


set posix 0


proc pass { msg } {
    global posix test

    if { $posix > 0 } {
	fail "WARNINGS/ERRORS occured: $msg"
    } else {
	puts "--- $test ---"
	puts "    PASS $msg"
	puts ""
    }
    set posix 0
    return
}


proc fail { msg } {
    global posix test

    puts "--- $test ---"
    puts "    FAILED $msg"
    puts ""
    set posix 0
    return
}


proc perror { msg num } {
    global posix test
    
    puts ""
    puts "    ERROR $msg"
    puts ""
    if { $num > 0 } {
	incr posix
    }
    return
}


proc untested { msg } {
    global poisx test

    puts "--- $test ---"
    puts "    UNTESTED $msg"
    puts "" 
    return
}


proc unresolved { msg } {
    global posix test

    puts "--- $test ---"
    puts "    UNRESOLVED $msg"
    puts ""
    return
}


proc unsupported { msg } {
    global posix test
    puts "--- $test ---"
    puts "    UNSUPPORTED $msg"
    puts ""
    return
}


proc note { msg } {
    global test
    puts "--- $test ---"
    puts "    NOTE: $msg"
    puts ""
    return
}


proc verbose { msg num } {
    global test
    if { $num > 0 } {
	puts stderr "--- $test ---"
	puts stderr "    MESSAGE: $msg"
	puts stderr ""
    }
    return
}


proc warning { msg num } {
    global posix test
    puts "--- $test ---"
    puts "    WARNING: $msg"
    puts ""
    if { $num > 0 } {
	incr posix
    }
    return
}

