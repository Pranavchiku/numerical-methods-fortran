!-----------------------------------------------------------------------
! Program to plot The Lotka-Volterra equations example
!
! Licensing: This code is distributed under the GNU GPL license.
! Author: Carlos Planelles Alemany, planelles20(at)gmail(dot)com
!-----------------------------------------------------------------------


program plot_ode_fun1
use module_edo, only: rk4
use lotka_volterra_function, only: lvf2
implicit none

real :: y0(2), a = 0.0, b = 440.0
real, allocatable :: x(:)
integer :: i, N = 4000
real, allocatable :: y(:,:)
integer :: IER, PGBEG

allocate(x(N))

! IER = PGBEG(0,'?',1,1)
IER = 1
if (IER.NE.1) stop

y0(:) = [10.0, 5.0]

y = rk4(lvf2, a, b, N, y0)

print *, sum(y)
if ( abs(sum(y) - 95576.2031) > 1e-8 ) stop

do i=1,N
    x(i) = a + (b-a)/(N-1)*(i-1)
end do

print *, sum(x)
if ( abs(sum(x) - 880000.000) > 1e-8 ) stop

! call PGENV(0.0, 40.0, 0.0, 11.0, 0, 1)
! call PGLAB('(y1)', '(y2)', 'Lotka-Volterra equation')
! CALL PGSCI(2)
! call PGLINE(N,y(1,:),y(2,:))

! call PGEND
END
