f(x)=a*x**2+b*x+c

fit f(x) "r2.dat" via a,b,c

plot "r2.dat" with points, f(x)
