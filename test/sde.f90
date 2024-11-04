!-----------------------------------------------------------------------
! Program to test Stochastic Ordinary Differential Equation methods
!
! Licensing: This code is distributed under the GNU GPL license.
! Author: Carlos Planelles Alemany, planelles20(at)gmail(dot)com
!-----------------------------------------------------------------------

program ode
use module_sdo, only: W2RK
use stochastic_dynamical_systems_function, only: force_regulation, force_regulation_g
use module_probability_distribution, only: init_random_seed
implicit none

real :: y0(2), a = 0.0, b = 100.0
real, allocatable :: x(:)
integer :: i, N = 1000
real, allocatable :: y(:,:)

call init_random_seed()

! diverges as random values everytime
y0(:) = [0.0e-9, -10.0e-9] ! meters

y = W2RK(force_regulation, force_regulation_g, a, b, N, y0)

print *, y(2,:)

end program
