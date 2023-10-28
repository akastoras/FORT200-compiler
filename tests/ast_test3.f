$ Correct program with various functions and use of backpatching

integer a, b(10), c(20)
real e, f(10), g(10)
logical k(10)
complex l(12)
record
	integer a
	record 
		complex l(12) 
		record
			character h
		endrec m
		logical k
	endrec n
endrec o, p(3)

data a/5/, b/1, 2, 0b10, 4, 5, 6, 7, 8/
data e/4.0/
data k/.TRUE., .FALSE., .TRUE./
data l/ (0.0:-4.1), (0.01:-3.9)/
data o/1, (3.4:-9.7), 'b', .true./

k = equal(a, b(1))

e = a ** t(f, g, a)

f(0) = a + t(f, g, square(a))

end

logical function equal(integer n, integer m)
equal = n .GT. m
end

integer function square(integer a)
square = a**2
end

real function t(real n(2), m(2) , integer x)
real e, f(10), g(10)
logical k(10)
complex l(12)
data e/4.0/
data k/.TRUE., .FALSE., .TRUE./
data l/ (0.0:-4.1), (0.01:-3.9)/

t = x ** (n(0)*m(1))
end



