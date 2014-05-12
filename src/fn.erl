-module(fn).
%% This is a bit fun-ny
%% ^That was lame

-export([comp/1, comp/2, comp/3, comp/4]).
-export([partial/2, partial/3, partial/4, partial/5]).
-export([pipe/3, pipe/4, pipe/5]).

comp(F, G) -> fun(X) -> F(G(X)) end.
comp(F, G, H) -> fun(X) -> F(G(H(X))) end.
comp(F, G, H, I) -> fun(X) -> F(G(H(I(X)))) end.
			    
comp(Fs) ->
    lists:foldl(fun comp/2, fun(X) -> X end, Fs).

partial(F, X) -> fun(Y) -> F(X, Y) end.
partial(F, X, Y) -> fun(Z) -> F(X, Y, Z) end.
partial(F, X, Y, Z) -> fun(A) -> F(X, Y, Z, A) end.
partial(F, A, B, C, D) -> fun(E) -> F(A, B, C, D, E) end.
				  
pipe(A, F, G) -> 
    Comp = comp(G, F),
    Comp(A).

pipe(A, F, G, H) ->
    Comp = comp(H, G, F),
    Comp(A).

pipe(A, F, G, H, I) ->
    Comp = comp(I, H, G, F),
    Comp(A).
