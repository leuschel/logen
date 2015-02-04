#!/bin/bash



sicstus --goal "set_prolog_flag(single_var_warnings,off), ensure_loaded('sicstus.pl'),ensure_loaded('socket.pl'),ensure_loaded('logen_main.pl'),save_program('logen_main.sav'),halt." 2>&1 | grep -v "^%" | grep -v "from clpq is private"


sicstus --goal "set_prolog_flag(single_var_warnings,off),prolog_flag(compiling,_,profiledcode),ensure_loaded('auto_bta.pl'),compile_bta,halt."  2>&1| grep -v "^%" | grep -v "from clpq is private"

#sicstus --goal "set_prolog_flag(single_var_warnings,off),ensure_loaded('auto_bta.pl'),compile_bta,halt."  2>&1 | grep -v "^%" | grep -v "from clpq is private"

#sicstus --goal "set_prolog_flag(single_var_warnings,off),ensure_loaded('global_bta_driver.pl'),compile_global_bta,halt."  2>&1| grep -v "^%" | grep -v "from clpq is private"


