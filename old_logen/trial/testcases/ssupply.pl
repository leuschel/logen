/* file: ssupply.pro */

ssupply( _snum, _pnum, _qty ) :-
    london_red_heavy_parts( _pnum, _pname, _weight ),
    best_suppliers( _snum, _sname, _status, _city ),
    supply( _snum, _pnum, _qty ).

best_suppliers( _snum, _sname, _status, _scity ) :-
    good_suppliers( _snum, _sname, _status, _scity ),
    good_cities( _city ),
    _city = _scity .

good_suppliers( _snum, _sname, _status, _city ) :-
    suppliers( _snum, _sname, _status, _city ),
    _status > 10 .

london_red_heavy_parts( _pnum, _pname, _weight ) :-
    london_red_parts( _pnum, _pname, _weight ),
    london_heavy_weights( _pno, _pname, _colour, _weight ),
    _pnum = _pno .

london_red_parts( _pnum, _pname, _weight ) :-
    red_parts( _pnum, _pname, _weight, _city ),
    _city = london .

red_parts( _pnum, _pname, _weight, _city ) :-
    parts( _pnum, _pname, _colour, _weight, _city ),
    _colour = red .

london_heavy_weights( _pnum, _pname, _colour, _weight ) :-
    heavy_weights( _pnum, _pname, _colour, _weight, _city ),
    _city = london .

heavy_weights( _pnum, _pname, _colour, _weight, _city ) :-
    parts( _pnum, _pname, _colour, _weight, _city ), 
    _weight > 10 .

good_cities( paris ).
good_cities( london ).
good_cities( hongkong ).
good_cities( regina ).
good_cities( saskatoon ).

supply( s1, p1, 300 ).
supply( s1, p2, 200 ).
supply( s1, p3, 400 ).
supply( s2, p1, 300 ).
supply( s2, p2, 400 ).
supply( s3, p1, 400 ).
supply( s4, p1, 200 ).
supply( s5, p1, 500 ).
supply( s5, p2, 400 ).

parts( p1, nut, red, 12, london ).
parts( p2, bolt, green, 17, paris ).
parts( p3, screw, blue, 17, rome ).

suppliers( s1, smith, 20, london ).
suppliers( s2, jones, 10, paris ).
suppliers( s3, blake, 30, paris ).
suppliers( s4, clark, 20, london ).
suppliers( s5, adams, 30, athens ).


