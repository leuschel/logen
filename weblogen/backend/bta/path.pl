%actual_path(F, N) :- name(F, Chars), append("/home/dre/public_html/", Chars, NC), name(N, NC).
actual_path(N, N).

ann_file(F, N) :- name(F, Chars), append(Chars, ".ann", NC), name(N, NC).

