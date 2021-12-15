syms a b c x y e m n v w z d t set rational

c = 0
a > 0
a < 1
b > 0
w > 0
v > 0
z > 0
d > 0


A = [v^2 a*v*w b*v*z; a*v*w w^2 c*w*z; b*v*z c*w*z z^2]
B = [2*d 0 0; 0 2*d 0; 0 0 2*d]

k = [m t e]
r = [m 0 0]

% S = inv((inv(A)+ B))*(inv(A)*r' + B*k')
% 
% S_2 = simplify(S,'Steps',5000)
% S_3 = collect(S_2,[m t e])

T = inv(inv(A)+ B)
T_2 = simplify(T,'Steps',100)
T_3 = collect(T_2, 1-b^2)

f = @(b)(T_3(1))
D = diff(f(b),b)
D_2 = collect(D,[b])
condition1 = D_2 < 0
C = solve(condition1, b ,'ReturnConditions',true)

Q = 4*a^2*d^2*v^2*w^2*z^2 + 2*a^2*d*v^2*w^2 - 4*d^2*v^2*w^2*z^2 - 2*d*v^2*w^2 - 2*d*v^2*z^2 - v^2 - (8*a^2*d^3*v^2*w^2*z^2 + 4*a^2*d^2*v^2*w^2 - 8*d^3*v^2*w^2*z^2 - 4*d^2*v^2*w^2 - 4*d^2*v^2*z^2 - 4*d^2*w^2*z^2 - 2*d*v^2 - 2*d*w^2 - 2*d*z^2 - 1)
E = solve(Q < 0,'ReturnConditions',true)