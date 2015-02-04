
:- module(logen_annotation_package_expand,[ ignore_ann/3 ],[ ]).

ignore_ann((:-ensure_loaded(logen_annotation)), [],_M).
ignore_ann(term_expansion(_,_), [], _Module).
ignore_ann((logen(_KeyWord,Head) :- Body), (Head :- NewBody), _M) :- remove_ann(Body, NewBody).

ignore_ann(logen(_KEYWORD, Call), Call, _M).
ignore_ann((:-residual(_)), [], _M).
ignore_ann((:-filter(_)), [], _M).
ignore_ann(end_of_file, end_of_file,_M).

remove_ann(logen(_Keyword, Call), Call).




