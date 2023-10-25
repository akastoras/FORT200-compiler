real a(5,3)
integer x

data a/1.0, 3.5, 6.4/

x = a(2,1)

end

integer function a(integer n, m)
integer i
i = n + m
$return
end