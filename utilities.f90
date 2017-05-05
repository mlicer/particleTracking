module utilities
! Module with some extra functions that come handy in particleTracking.f90.
! More info: matjaz.licer@mbss.org 

implicit none
!---------------------------------------------------


contains

subroutine num2str(num_in,str_out)
	integer :: num_in
	character*(*) :: str_out

	write(str_out , *) num_in
end subroutine num2str
!---------------------------------------------------

subroutine istr2num(str_in,num_out)
	integer, intent(out) :: num_out
	character*(*), intent(in) :: str_in

	read(str_in , *) num_out
end subroutine istr2num

subroutine rstr2num(str_in,num_out)
	real, intent(out) :: num_out
	character*(*), intent(in) :: str_in

	read(str_in , *) num_out
end subroutine rstr2num
!---------------------------------------------------

subroutine rflipVertical(nx,ny,array_in,array_out)
integer :: nx,ny,i,j
real, dimension(nx,ny), intent(in) :: array_in(:,:)
real, dimension(nx,ny), intent(out) :: array_out(:,:)

! print *,"flipVertical_real"

	do i = 1,nx
		do j = 1,ny
			array_out(i,j) = array_in(i,ny-j+1)
		enddo
	enddo
end subroutine rflipVertical
!---------------------------------------------------

subroutine iflipVertical(nx,ny,array_in,array_out)
integer :: nx,ny,i,j
integer, dimension(nx,ny), intent(in) :: array_in(:,:)
integer, dimension(nx,ny), intent(out) :: array_out(:,:)

! print *,"flipVertical_int"
	do i = 1,nx
		do j = 1,ny
			array_out(i,j) = array_in(i,ny-j+1)
		enddo
	enddo
end subroutine iflipVertical
!---------------------------------------------------

subroutine rflipHorizontal(nx,ny,array_in,array_out)
integer :: nx,ny,i,j
real, dimension(nx,ny), intent(in) :: array_in(:,:)
real, dimension(nx,ny), intent(out) :: array_out(:,:)
	do i = 1,nx
		do j = 1,ny
			array_out(i,j) = array_in(nx-i+1,j)
		enddo
	enddo
end subroutine rflipHorizontal
!---------------------------------------------------

subroutine iflipHorizontal(nx,ny,array_in,array_out)
integer :: nx,ny,i,j
integer, dimension(nx,ny), intent(in) :: array_in(:,:)
integer, dimension(nx,ny), intent(out) :: array_out(:,:)
	do i = 1,nx
		do j = 1,ny
			array_out(i,j) = array_in(nx-i+1,j)
		enddo
	enddo
end subroutine iflipHorizontal
!---------------------------------------------------

  subroutine dump2txt2d(nx,ny,array,filename)
  character*(*) :: filename
  integer :: i,j,nx,ny
  real,dimension(nx,ny) :: array
  open(12345,file=trim(adjustl(filename)),form='formatted',status='unknown')
  do i=1,nx
    do j = 1,ny
      write(12345,*) array(i,j)
    enddo
  enddo
  close(12345)
  end subroutine dump2txt2d



end module utilities
