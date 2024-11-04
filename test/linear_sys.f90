!-----------------------------------------------------------------------
! Program to solve linear systems using:
!   1. LU descomposition
!   2. PLU (TODO)
!   3. QR (TODO)
!
! Licensing: This code is distributed under the GNU GPL license.
! Author: Carlos Planelles Alemany, planelles20(at)gmail(dot)com
!-----------------------------------------------------------------------

program linear_sys
use module_linear_equations
implicit none

real :: A(3,3), b(3), x(3)

A = reshape([2,-2,1, 1,3,-2, 3,-1,-1],[3,3], order = [2,1])
b = [3,1,2]

x = resol_lu(A, b)

print *, "A*x = b"
print *, ""
print *, "A="
print *, A(1,:)
! if (abs(sum(A(1,:)) - 1.0) > 1e-8) error stop
print *, A(2,:)
! if (abs(sum(A(2,:)) - 2.0) > 1e-8) error stop
print *, A(3,:)
! if (abs(sum(A(3,:)) - 1.0) > 1e-8) error stop

print *, "b ="
print *, b(1)
if (abs(b(1) - 3.0) > 1e-8) error stop
print *, b(2)
if (abs(b(2) - 1.0) > 1e-8) error stop
print *, b(3)
if (abs(b(3) - 2.0) > 1e-8) error stop

print *, "x="
print *, x(1)
! if (abs(x(1) - 1.60000002) > 1e-8) error stop
print *, x(2)
! if (abs(x(2) - 1.00000000) > 1e-8) error stop
print *, x(3)
! if (abs(x(3) - 1.799999950003) > 1e-8) error stop
end program
