package provide pkgLogenDialog 1.0

namespace eval LogenDialog {
    variable dialog

}

#
# Example 39-1
# Procedures to help build dialogs.
#

proc LogenDialog::Dialog_Create {top title args} {
    
    eval {toplevel $top} $args
    wm title $top $title
    set y [expr {[winfo rooty .] + [winfo height .]/3 }]
    set x [expr {[winfo rootx .] + [winfo width .]/4 }]
    wm geometry $top "+$x+$y"
    return 1
	
}
proc LogenDialog::Dialog_Wait {top varName {focus {}}} {
	upvar $varName var

	# Poke the variable if the user nukes the window
	bind $top <Destroy> [list set $varName cancel]

	# Grab focus for the dialog
	if {[string length $focus] == 0} {
		set focus $top
	}
	set old [focus -displayof $top]
	focus $focus
	catch {tkwait visibility $top}
	catch {grab $top}

	# Wait for the dialog to complete
	tkwait variable $varName
	catch {grab release $top}
	focus $old
}
proc LogenDialog::Dialog_Dismiss {top} {
        destroy $top
}


proc LogenDialog::Dialog_Prompt { string } {
	global prompt
	set f .prompt
	if [Dialog_Create $f "Prompt" -borderwidth 10] {
		message $f.msg -text $string -aspect 1000
		entry $f.entry -textvariable prompt(result)
		set b [frame $f.buttons]
		pack $f.msg $f.entry $f.buttons -side top -fill x
		pack $f.entry -pady 5
		button $b.ok -text OK -command {set prompt(ok) 1}
		button $b.cancel -text Cancel \
			-command {set prompt(ok) 0}
		pack $b.ok -side left
		pack $b.cancel -side right
		bind $f.entry <Return> {set prompt(ok) 1 ; break}
		bind $f.entry <Control-c> {set prompt(ok) 0 ; break}
	}
	set prompt(ok) 0
	Dialog_Wait $f prompt(ok) $f.entry
	Dialog_Dismiss $f

	if {$prompt(ok)} {
		return $prompt(result)
	} else {
		return {}
	}
}

proc LogenDialog::Dialog_Error {title String args} {
    
	global dialog_error
	set f .dialog_error
	if [Dialog_Create $f "Error" -borderwidth 10] {
	    image create photo err_icon -format gif -file "icons/stop.gif"
	    #labelframe $f.f -labelwidget [label $f.l -image err_icon] -labelanchor wn      	
	    frame $f.f -relief ridge
	    
	    label $f.f.img -image err_icon
	    label $f.f.lblTitle -text "$title" -font "Courier 16 bold" -justify left	    
	    text $f.f.lblMsg -font "Courier 12" -wrap word -state normal -height 10 -width 40 -relief sunken
	    $f.f.lblMsg insert 1.0 "$String"
	    

	    
	    grid rowconfigure $f.f 0 -minsize 10m
	    grid rowconfigure $f.f 1 -weight 1
	    grid rowconfigure $f.f 2 -minsize 5m

	    grid columnconfigure $f.f 0 -minsize 10m -weight 1
	    grid columnconfigure $f.f 1 -minsize 20m -weight 10


	    set bFrame [frame $f.buttons]
	    set index 0
	    foreach b $args {
		set buttonText [lindex $b 0]
		set buttonReturn [lindex $b 1]
		
		pack [button $bFrame.b$index -text $buttonText -command "set dialog_error(ok) $buttonReturn"] -side right
		set index [expr {$index + 1}]

	    }
	    
	    grid $f.f.img -row 0 -column 0 -stick news
	    grid $f.f.lblTitle -row 0 -column 1 -stick news
	    grid $f.f.lblMsg -row 1 -column 0 -columnspan 2 -stick new
	    
	    grid rowconfigure $f 0 -weight 1
	    grid columnconfigure $f 0 -weight 1
	    grid rowconfigure $f 1 -minsize 1m
	    grid rowconfigure $f 2 -minsize 10m

	    
	    grid $f.f -row 0 -sticky news
	    grid $bFrame -row 2 -sticky ens	    	    
	}
	set dialog_error(ok) 0
       
	Dialog_Wait $f dialog_error(ok)
	Dialog_Dismiss $f
	return  $dialog_error(ok)

}

#puts [Dialog_Error "Save?" "Changes are not saved.\nSave now?" "Save" "Cancel"]


