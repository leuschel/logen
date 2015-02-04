

gen_name(File,Filepl) :- atom_chars(File,AC), append(AC,['.',p,l],PLF),
                   atom_chars(Filepl,PLF).
                   
                   
test(X) :- gen_name(builtin_test,X), print(see(X)),nl,
           see(X), seen, print(done),nl.
