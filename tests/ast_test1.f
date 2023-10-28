$ Correct program of various declarations and initiliazations

integer a, b(10)
integer c(3,6,9), list d
real e, f(100,4), list list g
character h, i(8)
logical list j, k
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
data c/1, 2, 3, 0HDEADBEEF, 5, 6, 7 ,8, 9/
data e/4.0/, f/3.5, 0HA.12, 9.999, 12.54, 0HF.A1, 5E-2, 0B1.0101/
data h/'a'/, i/'a', 'b', 'c'/
data l/ (0.0:-4.1), (0.01:-3.9)/
data o/1, (3.4:-9.7), 'b', .true./

a = 0

end