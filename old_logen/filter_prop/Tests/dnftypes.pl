
denotes_z0(lit).
denotes_z1(lit).
denotes_z2(lit).
denotes_z3(lit).
denotes_z4(lit).
denotes_z5(lit).
denotes_z6(lit).
denotes_z7(lit).
denotes_z8(lit).
denotes_z9(lit).
denotes_s(lit,lit).
denotes_o(dnf,dnf,dnf).




dnf ->  z0 ... z9 | s(lit) | a(lit,term) | o(term,dnf)
term -> z0 ... z9 | s(lit) | a(lit,term) 
lit -> z0 ... z9 | s(lit)

