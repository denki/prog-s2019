edge(1,1).                            % e1
edge(1,4).                            % e2
edge(1,2).                            % e3
edge(3,2).                            % e4
edge(4,3).                            % e5

path(X,X).                            % p1
path(U,W) :- path(V, W), edge(U,V).   % p2
