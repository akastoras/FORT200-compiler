$ Program that uses an undeclared record field

integer b
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
data o/1, (3.4:-9.7), 'b', .true./

b = o.f

end
