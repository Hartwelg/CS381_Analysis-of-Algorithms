%Gauge Hartwell
%CS381 Lab5
:- consult('royal.pl').
%mother is the female parent.
%M is the mother of C if M is the parent of C and M is female.
mother(M,C):- parent(M,C), female(M).
%father is the male parent.
%M is the father of C if M is the parent of C and M is male.
father(M,C):- parent(M,C), male(M).
%spouse is whoever M is married to.
%M is the spouse of C if M is married to C or C is married to M.
spouse(M,C):- married(M,C); married(C,M).
%child is whoever C is a parent of.
%M is the child of C if C is the parent of M.
child(M,C):- parent(C,M).
%son is the male child.
%M is the son of C if M is the child of C and M is male.
son(M,C):- child(M,C), male(M).
%daughter is the female child.
%M is the daughter of C if M is the child of C and M is female.
daughter(M,C):- child(M,C), female(M).
% sibling is someone who shares both a mother and a father, but is not
% the same person.
% M is the sibling of C if M shares both a mother and a father with C,
% and M and C are not the same person.
sibling(M,C):- mother(Y,C), mother(Y,M), father(X,C), father(X,M), M \= C.
%brother is a male sibling.
%M is the brother of C if M is the sibling of C and M is male.
brother(M,C):- sibling(M,C), male(M).
%sister is a female sibling.
%M is the sister of C if M is the sibling of C and M is female.
sister(M,C):- sibling(M,C), female(M).
%uncle is the brother of a parent.
%M is the uncle of C if M is the brother of C's parent.
uncle(M,C):- brother(M,Y), parent(Y,C).
% M is uncle by marriage to C if they are married to someone who has a
% sibling with a child (C).
uncle(M,C):- spouse(M,Y), sibling(Y,X), child(C,X), M \= C.
%aunt is the sister of a parent.
%M is the aunt of C if M is the sister of C's parent.
aunt(M,C):- sister(M,Y), parent(Y,C).
% M is aunt by marriage to C if they are married to someone who has a
% sibling with a child (C).
aunt(M,C):- spouse(M,Y), sibling(Y,X), child(C,X), M \= C.
%grandparent is the parent of a parent.
% M is the grandparent of C if M is the parent of Y and Y is the parent
% of C.
grandparent(M,C):- parent(M,Y), parent(Y,C).
%grandfather is the father of a parent.
% M is the grandfather of C if M is the father of Y and Y is the parent
% of C.
grandfather(M,C):- father(M,Y), parent(Y,C).
%grandmother is the mother of a parent.
% M is the grandmother of C if M is the mother of Y and Y is the parent
% of C.
grandmother(M,C):- mother(M,Y), parent(Y,C).
%grandchild is the child of a child.
% M is the grandchild of C if M is the child of Y and Y is the child of
% C.
grandchild(M,C):- child(M,Y), child(Y,C).
%an ancestor is a parent, or an ancestor of a parent.
ancestor(M,C):- parent(M,C).
ancestor(M,C):- parent(M,Y), ancestor(Y,C).
%a descendant is a child, or the child of a descendent.
descendant(M,C):- child(M,C).
descendant(M,C):- child(M,Y), descendant(Y,C).
%M is older than C if M was born before C.
older(M,C):- born(M,X), born(C,Y), X < Y.
%M is younger than C if M was born after C.
younger(M,C):- born(M,X), born(C,Y), X > Y.
%M is the regentWhenBorn if M reigned when C was born.
regentWhenBorn(M,C):- reigned(M,Y,Z), born(C,W), W<Z, W>Y.
%cousin in the child of the sibling of the parent
% M is the cousin of C if C is the child of X, X is the sibling of Y,
% and Y is the parent of M
cousin(M,C):- parent(Y,M), sibling(X,Y), child(C,X).

portray(Term):- atom(Term), format("~s", Term).
