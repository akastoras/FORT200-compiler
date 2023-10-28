$ Correct program with various assignments and expression uses

integer a, b(10)
integer list d
real e, list g
character h, i(8), list list r
logical list j, k, q
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
data h/'a'/, i/'a', 'b', 'c'/
data l/ (0.0:-4.1), (0.01:-3.9)/
data o/1, (3.4:-9.7), 'b', .true./

a = ((a + b(2))*2)**3 - CADR([1, 2, 3, 4])
e = (a/4.6)**8.43
l(0) = (-3.4:0.9)**o.a + CAR(g)
k = a .GT. e .AND. CAR(j)
q = h .EQ. i(3-2)
k = o.n.k
l(1) = o.n.l(0)

b(+1) = LENGTH([-1, -23, -3, -4])
d = NEW(2+9)
r = NEW(['o', 'k'])
end