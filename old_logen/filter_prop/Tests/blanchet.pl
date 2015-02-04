attacker(pencrypt(M,PK)) :-
	attacker(M),
	attacker(PK).
attacker(pk(SK)) :-
	attacker(SK).
attacker(M) :-
	attacker(pencrypt(M,pk(SK))),
	attacker(SK).
attacker(sign(M,SK)) :-
	attacker(M),
	attacker(SK).
attacker(M) :-
	attacker(sign(M,SK)).
attacker(sencrypt(M,K)) :-
	attacker(M),
	attacker(K).
attacker(M) :-
	attacker(sencrypt(M,K)),
	attacker(K).
attacker(pk(skA)).
attacker(pk(skB)).
attacker(a).

attacker(pencrpyt(sign(k(pk(X)),skA),pk(X))) :-
	attacker(pk(X)).
attacker(sencrpyt(s,K1)) :-
	attacker(pencrpyt(sign(K1,skA),pk(skB))).

unsafe :-
	attacker(s).
