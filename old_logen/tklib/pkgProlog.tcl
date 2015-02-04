package provide pkgProlog 1.0

namespace eval Prolog {
    variable prologSocket

}

# initialise the Prolog  engine to localhost and write port into file
proc procProlog {} {
    global prologSocket
    if { [file exists prologport] } {
	file delete prologport
    }
    #should remove existing port file and then wait on it to reapper....
    exec "sicstus" "-l" "socket.pl" "--goal" "prolog_socket,halt." 2> prologlog > prologstdout &

    # wait for port number to be written
    while { ![file exists prologport] } {
	after 200
    }


    set portfile [open "prologport"]
    set port [gets $portfile]
    set prologSocket [socket localhost $port]
    fconfigure $prologSocket -buffering line
    return $prologSocket
}

# interpet packet reply
proc procGetAns {s} {
    set ans ""
    set prev ""
    set c [read $s 1]    
    while { 1 } {
	#puts $ans
	if { $prev == "\001" && $c == "\001" } {
	    break
	}
	
	append ans $c	
	set prev $c
	set c [read $s 1]
    }
    return $ans    
}

# make a prolog call and set prolog_variables to the correct vars
proc prolog {call} {
    global prologSocket
    #global prolog_variables
    upvar 1 prolog_variables prolog_variables

    puts "DEBUG calling prolog $call"
    if  { [lindex [split [string trim $call] {} ] end] != "." } {
	append call "."
    }


    
    if { ![info exists prologSocket] } {
	procProlog
    }

    puts $prologSocket $call
    set ans [procGetAns $prologSocket]
    set anslist [split $ans "\001"]
    puts "Command is [lindex $anslist 0]"
    if { [lindex $anslist 0] == "OK--" } {
	foreach var $anslist {
	    set map [split $var "\002"]
	    if { [llength $map] == 2 } {
		set prolog_variables([lindex $map 0]) [lindex $map 1]	
	    }

	}
    } elseif { [lindex $anslist 0] == "FAIL" } {
	return 0
    } else {
	error $anslist
    }

    return 1

}


proc test {} {
    global prolog_variables
    prolog "print(connected_to_prolog), X=a,Y=j."
    puts "\nprolog_variables(X) == $prolog_variables(X)"


    prolog "print(connected_to_prolog), X=x_val,Y=y_val."
    puts "\nprolog_variables(X) == $prolog_variables(X)"
}

#test