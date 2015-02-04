:- module(msg,[msg/3]).

/* msg.pro */
/********************************************************************************/
/* 
MODULE MSG
This module definines the construct_msg, is_instance_of, is_variant_of, 
not_variant_of, less_general, more_general and fresh_term predicates, which 
play an important role in many modules of the PDP program.
*/
/********************************************************************************/

%:- use_module(runtime_checks).
:- include(runtime_checks_perform).

msg(X,Y,MSG) :- mnf(construct_msg(X,Y,MSG)).


%:- hide.


/********************************************************************************/
/* CONSTRUCT MSG OF TWO ATOMS */
/* The procedure constructs the MSG for the input pair of atoms.
  The difficulty of the computation stands in the fact that the msg can not
  simply be computed by pairwise observation of corresponding subterms.
  Look the following examples:
  msg( atom(aa,aa), atom(bb,bb) ) = atom(_x,_x)
  msg( atom(aa,aa), atom(_a,_a) ) = atom(_x,_x)
  Considering only pairs of subterms we can only yield:  atom(_a,_b)
  So a refinement is needed to provide the theoretical MSG.

  The solution is the following: We have to registrate all the subterm pairs
  along with the assigned "subMSGs" that we evaluated so far during the process.
  If we come to a new pair of subterms, we look back, whether there has been 
  a subMSG to the same pair already assigned. If yes, we assign the same
  subMSG to the pair. The pair of subterms and their subMSG is held in a
  triplet structure: triplet( term1, term2, subMSG ).

  Another tricky case:
  msg( atom(aa,bb), atom(_a,_a) ) = atom(_x,_y)
  Nothing extra has to be done to yield this result.
*/


construct_msg( _term1, _term2, _msg ):-
    msg_of_pair( _term1, _term2, _msg, [], _ ).


msg_of_pair( _term1, _term2, _msg, _record, _record ):-
    _triplet = triplet( _term1, _term2, _msg ),
    triplet_recorded( _record, _triplet ), !.
                /* The same pair already occured, MSG fetched from the record. */
msg_of_pair( _term1, _term2, _msg, _record, [_triplet|_record] ) :- 
    atomic( _term1 ), atomic( _term2 ),
    _term1 == _term2, !,                                 /* The same constants.*/
    _msg = _term1,
    _triplet = triplet( _term1, _term2, _msg ).
msg_of_pair( _term1, _term2, _msg, _inrec, [_triplet|_outrec] ) :-
    nonvar( _term1 ), nonvar( _term2 ),
    functor( _term1, _functor, _arity ),
    functor( _term2, _functor, _arity ), !,        /* The same functor & arity */
    _term1 =.. [_|_subterms1],
    _term2 =.. [_|_subterms2],
    msg_of_pairs( _subterms1, _subterms2, _submsgs, _inrec, _outrec),
    _msg =.. [_functor|_submsgs],
    _triplet = triplet( _term1, _term2, _msg ).
msg_of_pair( _term1, _term2, _fresh, _record, [_triplet|_record] ):-
    _triplet = triplet( _term1, _term2, _fresh ).


msg_of_pairs( [], [], [], _record, _record ).
msg_of_pairs( [_term1|_rest1], [_term2|_rest2], [_msg|_msgs], _inrec, _outrec ):-
    msg_of_pair( _term1, _term2, _msg, _inrec, _tmprec ),
    msg_of_pairs( _rest1, _rest2, _msgs, _tmprec, _outrec ).


triplet_recorded( [_triplet|_], _template ):-
    _triplet =  triplet( _term1, _term2, _submsg ),
    _template = triplet( _term3, _term4, _submsg ),
    _term1 == _term3,
    _term2 == _term4,
    !.
triplet_recorded( [_|_triplets], _template ):-
    triplet_recorded( _triplets, _template ).




/********************************************************************************/
/* IS_INSTANCE_OF, NOT_INSTANCE_OF, IS_VARIANT_OF, NOT_VARIANT_OF, 
  IS_LESS_GENERAL, IS_MORE_GENERAL PREDICATES */
/* If the term in the first argument is an instance of the term in the second
  argument, the predicate succeeds, otherwise fails. There are some tricky
  issues here again with the multiply occuring variables:
  a(aa,bb), (aa,_y) and a(_y,_z) are NOT instances of a(_x,_x), since no
  substitution can be given for a(_x,_x) to make them identical.

  To keep track of variables, a list of variable-substitutions is recorded
  during the subterm-by-subterm checking. Every variable in the second term
  is first checked in this list, whether it has occured. To be able to decide
  whether the first term is a variant or a "real" instance (a less general term)
  of the second one, a flag is set to have the "less_general" binding, if any
  of the subterms is proved to be less general than its pair in the first term.

  Care should be taken to be able to recognize that a(_x,_x) is less general 
  than a(_y,_z) ! See the point marked by *** !
*/

is_instance_of( _term1, _term2 ):-
    msg_instance_of( _term1, _term2, [], _, _ ).

not_instance_of( _term1, _term2 ):-
    msg_instance_of( _term1, _term2, [], _, _ ),
    !, fail.
not_instance_of( _, _ ).


is_variant_of( _term1, _term2 ):-
    msg_instance_of( _term1, _term2, [], _, _flag ),
    _flag \== less_general.


is_combi_variant_of( _mask, _term1, _term2 ):-
    combi_instance_of( _term1, _term2, [], _, _flag, _mask ),
    _flag \== less_general.


combi_instance_of( _term1, _term2, _record, _record, _, _mask ):-
    atomic(_term2), !, _term1 == _term2. 
combi_instance_of( _term1, _term2, _inrec, _outrec, _flag, _mask ):-
    nonvar( _term2 ), functor( _term2, _functor, _arity ),
    nonvar( _term1 ), functor( _term1, _functor, _arity ),
    _term1 =.. [ _| _subterms1 ],
    _term2 =.. [ _| _subterms2 ],
    _mask = atomstr( _, _maskarg, _ ),
    writeq(_maskarg),nl,
    combi_instance_subterms( _subterms1, _subterms2, _inrec, _outrec, _flag, _maskarg ).



combi_instance_subterms( [], [], _record, _record, _, _maskarg ).
combi_instance_subterms( [ _term1| _terms1], [ _term2| _terms2], _inrec, _outrec, _flag, [_mask1| _masks] ):-
    _mask1 = argstr( _, _, _x ), atomic( _x ), 
    !,
    combi_instance_subterms( _terms1, _terms2, _inrec, _outrec, _flag, _masks ).
combi_instance_subterms( [ _term1| _terms1 ], [ _term2| _terms2], _inrec, _outrec, _flag, [ _mask1| _masks] ):-
    combi_instance_subterms( _terms1, _terms2, _inrec, _tmprec, _flag, _masks ),
    msg_instance_of( _term1, _term2, _tmprec, _outrec, _flag ).


not_variant_of( _term1, _term2 ):-
    msg_instance_of( _term1, _term2, [], _, _flag ),
    _flag \== less_general,
    !, fail.
not_variant_of( _, _ ).


is_less_general( _term1, _term2 ):-
    msg_instance_of( _term1, _term2, [], _, _flag ),
    _flag == less_general.

is_more_general( _term1, _term2 ):-
    msg_instance_of( _term2, _term1, [], _, _flag ).

  
msg_instance_of( _term1, _term2, _record, _record, _ ):-
    subst_recorded( _term2, _record, _term3 ), !, _term1 == _term3.
msg_instance_of( _term1, _term2, _record, _record, _ ):-
    atomic( _term2 ), !, _term1 == _term2.
msg_instance_of( _term1, _term2, _inrec, _outrec, _flag ):-
    nonvar( _term2 ), functor( _term2, _functor, _arity ), !,
    nonvar( _term1 ), functor( _term1, _functor, _arity ),
    _term1 =.. [_|_subterms1],
    _term2 =.. [_|_subterms2], 
    instance_subterms( _subterms1, _subterms2, _inrec, _outrec, _flag ).
msg_instance_of( _term1, _term2, _record, [_subst|_record], less_general ):-
    var(_term2), nonvar(_term1), !, 
    _subst = subst(_term2,_term1).
msg_instance_of( _term1, _term2, _record, [_subst|_record], less_general ):-
    var(_term2), var(_term1),
    recorded_subst(_term1, _record, _), !, 
	   /* *** term1 variable already substituted into an other variable.*/
    _subst = subst(_term2,_term1).
msg_instance_of( _term1, _term2, _record, [_subst|_record], _ ):-
    var(_term2), var(_term1), 
    _subst = subst(_term2,_term1).


instance_subterms( [], [], _record, _record,  _ ).
instance_subterms( [_term1|_terms1], [_term2|_terms2], _inrec, _outrec, _flag ):-
    instance_subterms( _terms1, _terms2, _inrec, _tmprec, _flag ),
    msg_instance_of( _term1, _term2, _tmprec, _outrec, _flag ).


subst_recorded( _term2, [_subst|_], _term3 ):-
    _subst = subst(_t2,_t3),
    _term2 == _t2, !, _term3 = _t3.
subst_recorded( _term2, [_|_substs], _term3 ):-
    subst_recorded( _term2, _substs, _term3 ).

recorded_subst( _term1, [_subst|_], _term3 ):-
    _subst = subst(_t3,_t1),
    _term1 == _t1, !, _term3 = _t3.
recorded_subst( _term1, [_|_substs], _term3 ):-
    subst_recorded( _term1, _substs, _term3 ).





/********************************************************************************/
/* CONSTRUCT VARIABLE AND SUBSTITUTION LIST */

construct_subst_list( _atom, _instatom, _varlist, _substlist ) :-
    msg_instance_of( _instatom, _atom, [], _record, _ ),
    split_record( _record, _varlist, _substlist ).

split_record( [], [], [] ).
split_record( [_substrec|_record], [_var|_varlist], [_subst|_substlist] ):-
    _substrec = subst( _var, _subst ),
    split_record( _record, _varlist, _substlist ).



/********************************************************************************/
/* CONSTRUCT FRESH ATOM */

fresh_atom( _atom, _freshatom ):-
    fresh_term( _atom, _freshatom, [], _ ).
    
fresh_term( _term, _freshterm, _record, _record ):-
    subst_recorded( _term, _record, _freshterm ), !.
fresh_term( _term, _freshterm, _record, _record ):-
    atomic( _term ), !, _freshterm = _term.
fresh_term( _term, _freshterm, _inrec, _outrec ):-
    nonvar( _term ), functor( _term, _functor, _ ), !,
    _term =.. [_|_subterms],
    fresh_subterms( _subterms, _freshsubterms, _inrec, _outrec ),
    _freshterm =.. [_functor|_freshsubterms].
fresh_term( _term, _freshvar, _record, [_subst|_record] ):-
    var( _term ), !,
    _subst = subst( _term, _freshvar ).

fresh_subterms( [], [], _record, _record ).
fresh_subterms( [_term|_rest], [_freshterm|_freshrest], _inrec, _outrec ):-
    fresh_term( _term, _freshterm, _inrec, _temprec ),
    fresh_subterms( _rest, _freshrest, _temprec, _outrec ).


/********************************************************************************/
/********************************************************************************/
