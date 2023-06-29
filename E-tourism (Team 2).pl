:-style_check(-discontiguous).
:-style_check(-singleton).
%facts

% offerMean(X, Y) -> Transportation mean Y is used with offer X

offerMean(offer(dahab, [diving, snorkeling, horseRiding], 10000, 2020-02-12, 2020-03-12, period(2020-03-15, 2020-04-15), 10, 5), bus).
offerMean(offer(taba, [diving], 1000, 2020-02-12, 2020-03-12, period(2020-06-01, 2020-08-31), 10, 4), bus).

% offerAccommodation(X, Y) -> Accommodation Y is part of offer X

offerAccommodation(offer(dahab, [diving, snorkeling, horseRiding], 10000, 2020-02-12, 2020-03-12, period(2020-03-15, 2020-04-15), 10, 5), hotel).
offerAccommodation(offer(taba, [diving], 1000, 2020-02-12, 2020-03-12, period(2020-06-01, 2020-08-31), 10, 4), hotel).

% customerPreferredActivity(X, Y, R) -> Y is the preferred activity kind wrt customer X with relevance R

customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), diving, 100).
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), snorkeling, 100).
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), horseRiding, 20).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), snorkeling, 60).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), diving, 20).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), horseRiding, 50).

% customerPreferredMean(X, Y, R) -> Y is the preferred transportaion mean wrt customer X with relevance R

customerPreferredMean(customer(ahmed, aly, 1993-01-30, single, 0, student), bus, 100).
customerPreferredMean(customer(mohamed, elkasad, 1999-01-30, single, 0, student), bus, 10).

% customerPreferredAccommodation(X, Y, R) -> Y is the preferred accommodation to customer X with relevance R

customerPreferredAccommodation(customer(ahmed, aly, 1993-01-30, single, 0, student), hotel, 20).
customerPreferredAccommodation(customer(mohamed, elkasad, 1999-01-30, single, 0, student), hotel, 100).

/*predicates:-- 
	
possibleSubset: if all instances of variable R are available in list L */

possibleSubset([],[]).
possibleSubset([H|T],R):-
			possibleSubset(T,R).
possibleSubset([H|T],R) :- 
			possibleSubset(T,W), 
			remove(H,R,W).
remove(X,[X|T],T).
remove(X,[F|T],[F|T1]) :- remove(X,T,T1).

/*choosePrefrence: tries to satistfy as many prefrences as possible*/

choosePrefrences([], []).

choosePrefrences([activity([X|Y])|T],[activity(R)|T]):-
	possibleSubset([X|Y],R).

choosePrefrences([H|T],[H|T2]) :-
	choosePrefrences(T,T2).

choosePrefrences([_|T],T2) :-
    choosePrefrences(T,T2).


/*prefrence Satisfaction: how much is the customer satisfied with the offer*/

prefrenceSatisfaction(_,C,L,S):-
		test1(C,L,0,S1),
		test2(C,L,S2),
		test3(C,L,S3),
		S is S1+S2+S3.
test1(C,[H|T],N,S1):-
		 H \= activity(_),
		 test1(C,T,N,S1).
test1(_,[activity([])|_],S,S).
test1(C,[activity([H|T])|_],Acc,S):-
		 customerPreferredActivity(C,H,S4),
		 S1 is S4+Acc,
		 test1(C,[activity(T)|_],S1,S).
test1(C,[activity([H|T])|_],Acc,S1):-
		 \+ customerPreferredActivity(C,H,_),
		 test1(C,[activity(T)|_],Acc,S1).
test1(_,[],_,0).

test2(C,[H|T],S2):-
		 H \= mean(_),
		 test2(C,T,S2).
test2(C,[mean(X)|_],S2):- 
		 customerPreferredMean(C,X,S5),
		 S2 = S5.
test2(C,[mean(X)|_],S2):-
		 \+ customerPreferredMean(C,X,_),
		 S2 = 0.
test2(_,[],0).
test3(C,[H|T],S3):-
		 H \= accommodation(_),
		 test3(C,T,S3).
test3(C,[accommodation(X)|_],S3):-
		 customerPreferredAccommodation(C,X,S6),
		 S3 = S6.
test3(C,[accommodation(X)|_],S3):-
		 \+ customerPreferredAccommodation(C,X,_),
		 S3 = 0.
test3(_,[],0).

/* Overlap period: checks if two dates are overlapping */

smaller(Y1-M1-D1,Y1-M1-D2):-
		 D1=<D2.
smaller(Y1-M1-_,Y1-M2-_):-
		 M1<M2.
smaller(Y1-_-_,Y2-_-_):-
		 Y1<Y2.


overlapPeriod(period(Y1-M1-D1,Y2-M2-D2),period(Y3-M3-D3,Y4-M4-D4)):-
		 smaller(Y3-M3-D3,Y1-M1-D1),
		 smaller(Y1-M1-D1,Y4-M4-D4),
		 !.
overlapPeriod(period(Y1-M1-D1,Y2-M2-D2),period(Y3-M3-D3,Y4-M4-D4)):-
		 smaller(Y1-M1-D1,Y3-M3-D3),
		 smaller(Y3-M3-D3,Y2-M2-D2),
		 !.
				
/*getOffer*/

getOffer(L,offer(D,A,B,F,T,period(X,Y),N,U)):-
		 offerMean(offer(D,A,B,F,T,period(X,Y),N,U),M),
		 getOffer1(L,M),
		 offerAccommodation(offer(D,A,B,F,T,P,N,U),A1),
		 getOffer2(L,A1),
		 getOffer3(L,period(X,Y)),
		 getOffer4(L,D),
		 getOffer5(L,A).
getOffer4([dest(X)|_],O):-
		 X=O.
getOffer4([H|T],O):-
		 H\=dest(_),
		 getOffer4(T,O).
getOffer4([],O).
getOffer5([activity([H|T])|_],O):-
		 choosePrefrences(O,[H|T]).
getOffer5([H|T],O):-
		 H\=activity(_),
		 getOffer5(T,O).
getOffer5([],O).
getOffer3([period(X,Y)|_],O):-
		 overlapPeriod(period(X,Y),O).
getOffer3([H|T],O):-
		 H\=period(X,Y),
		 getOffer3(T,O).
getOffer3([],O).
getOffer1([mean(X)|_],X).
getOffer1([H|T],O):-
		 H\=mean(_),
		 getOffer1(T,O).
getOffer1([],O).
getOffer2([accommodation(X)|_],O):-
		 X=O.
getOffer2([H|T],O):-
		 H\=accommodation(_),
		 getOffer2(T,O).
getOffer2([],O).

/*recommendOfferForCustomer:*/

recommendOfferForCustomer([H|T],L,O):-
		 choosePrefrences([H|T],L),
		 getOffer(L,O).

/*recommendOffer: an offer recommened for the customer according to their prefrences*/
insert_sort(List,Sorted):-
	i_sort(List,[],Sorted),
	!.

i_sort([],Acc,Acc).
i_sort([H|T],Acc,Sorted):-
	insert(H,Acc,NAcc),i_sort(T,NAcc,Sorted).
   
insert(X,[Y|T],[Y|NT]):-
	X>Y,insert(X,T,NT).
insert(X,[Y|T],[X,Y|T]):-
	X=<Y.
insert(X,[],[X]).

indexOf([Element|_], Element, 0):- !.
indexOf([_|Tail], Element, Index):-
  indexOf(Tail, Element, Index1),
  !,
  Index is Index1+1.
 


satHelper([C|Tc],[P|Tp],offer(D,A,B,F,T,P1,N,U),R,[C|L]):-
	prefrenceSatisfaction(offer(D,A,B,F,T,P1,N,U),C,P,S),
	insert_sort([S|Ts],Tsx),
	appHelper(U,X,Tsx,[S|Ts],R),
	custHelper(C,S,R,[C|L]),
	satHelper(Tc,Tp,offer(D,A,B,F,T,P1,N,U),R,L).

custHelper([C|Tc],S,[T|Z],[G|H]):-
	indexOf([T|Z],S,I),
	nth0(I,L,C,[G|H]).
	

appHelper(U,1,[D|Td],[S|Ts],[R|Tr]):-
	append([D|Td],[S|Ts],[R|Tr]),
	appHelper(offer(_,_,_,_,_,_,_,U),X1,Td,[R|Tr],R2),
	X1 is X1+1.

recommendOffer([],_,_,[]).

recommendOffer([C|T1],[P|T2],offer(D,A,B,F,T,P1,N,U),[C|L]):-
		getOffer(P,offer(D,A,B,F,T,P1,N,U)),
		satHelper([C|T1],[P|T2],offer(D,A,B,F,T,P1,N,U),_,G),
		recommendOffer(T1,T2,offer(D,A,B,F,T,P1,N,U),G).

recommendOffer([C|T1],[P|T2],offer(D,A,B,F,T,P1,N,U),L):-
	\+ getOffer(P,offer(D,A,B,F,T,P1,N,U)),
	recommendOffer(T1,T2,offer(D,A,B,F,T,P1,N,U),L).

%prolog test
getAllActivities(L):-
	setof(X,Y^Z^customerPreferredActivity(Y,X,Z),L).

mostPreferredActivity(C,A):-
	getAllActivities(L),
	helper(C,L,A,_),
	max(X1,A1,X2,A2,A1).
	
	
helper(C,[X|[]],X,S):-
	customerPreferredActivity(C,X,S).
	
max(X1,A1,X2,A2,A1):-
	X1>=X2.
max(X1,A1,X2,A2,A2):-
	X2>=X1.
	

	
