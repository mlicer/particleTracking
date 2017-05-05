program test

integer, allocatable :: i(:)


allocate(i(1))
i=5
print *,i
print *,i(1)

end
