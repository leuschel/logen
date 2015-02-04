package provide pkgLogenParser 1.0

namespace eval LogenParser {
 #   variable dialog
}

proc LogenParser::parseProlog { buffer filename } {
    
    prolog "parse_file_syntax('$filename',Syntax)"
    set lFormat $prolog_variables(Syntax)    

    if { [lindex $lFormat 0] != "error" } {
		procFormatSource $lFormat $buffer		
		return 1
    } else {
		set err_position [lindex $lFormat 1]
		$buffer.text mark set err "1.0 + $err_position chars"
		$buffer.text tag add errorTag "err linestart" "err lineend"
		procShowErrors
		return 0
    }       
}


proc LogenParser::procFormatSource {lFormat Frame} {

	set oldstate [$Frame.text cget -state]

	$Frame.text configure -state normal
	for {set i 0} { $i < [llength $lFormat] } {set i [expr $i + 3] } {
		set Start  [lindex $lFormat [expr $i]]
		set End  [lindex $lFormat [expr $i +1]]
		set Tag  [lindex $lFormat [expr $i +2]]

		set quote [$Frame.text get "0.0 + $End chars - 2 chars" "0.0 + $End chars - 1 chars"]
		# This is a nasty hack which means that it highlights 'C' completely
		if { $quote == "'" } {
			set Start [expr $Start - 2]
		}
		$Frame.text tag add [lindex $lFormat [expr $i + 2]] "0.0 + $Start chars - 1 chars" "0.0 + $End chars - 1 chars"

	}

	$Frame.text configure -state "$oldstate"
}

