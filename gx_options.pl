

usage('Usage: gxfile [Options] "Atom."').

%%% these are the options processes by the standalone gx files
recognised_option('--help',help,[],'Prints this message').
recognised_option('-o',outfile(Out),[Out],'Specialised filename').
recognised_option('--add_entry',add_entry,[],'Generate entry point').


