!-----------------------------------------------------------------------
! Program to solve ordinary differential equations
!
! Licensing: This code is distributed under the GNU GPL license.
! Author: Carlos Planelles Alemany, planelles20(at)gmail(dot)com
!-----------------------------------------------------------------------

program ode
use module_edo, only: AB2
use whatever_function, only: odef1
implicit none

real :: y0(3), a = 0.0, b = 1.0
integer :: i, N = 10
real, allocatable :: y(:,:)

y0(:) = [1.0, 1.0, 1.0]

y = AB2(a, b, N, y0)
print *, sum(y)
if (abs(sum(y) - 30.3195057) > 1e-8) error stop
do i = 1, N
    print *, y(:,i)
end do

end program
