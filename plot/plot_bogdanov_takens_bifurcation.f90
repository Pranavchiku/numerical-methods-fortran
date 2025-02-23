!-----------------------------------------------------------------------
! simulation transesterification reaction at 50 C
!
! Licensing: This code is distributed under the GNU GPL license.
! Author: Carlos Planelles Alemany, planelles20(at)gmail(dot)com
!-----------------------------------------------------------------------


program plot_ode_fun1
use module_edo, only: AB5
use dynamical_systems_function, only: bogdanov_takens_bifurcation
use ieee_arithmetic, only: ieee_is_nan, ieee_is_finite
implicit none

real :: a = 0.0, b = 25.0
real, allocatable :: t(:)
integer :: i, j, N = 2000, Mx = 50, My = 4, k
real, allocatable :: y(:,:,:,:), y0(:,:,:)
real :: sum_tmp

integer :: IER, PGBEG

allocate(t(N), y(Mx, My,2,N), y0(Mx, My,2))

! IER = PGBEG(0,'?',1,1)
IER = 1
if (IER.NE.1) stop

!initial concentration
! Triglycerides = 1.0 mol
! Alcohol = 6.0 mol

do i = 1, Mx
    y0(i,1,:) = [10*real(i)/Mx-5, -5.0]
    y(i,1,:,:) = AB5(bogdanov_takens_bifurcation, a, b, N, y0(i,1,:))
    y0(i,2,:) = [10*real(i)/Mx-5, 5.0]
    y(i,2,:,:) = AB5(bogdanov_takens_bifurcation, a, b, N, y0(i,2,:))
    y0(i,3,:) = [-5.0, 10*real(i)/Mx-5]
    y(i,3,:,:) = AB5(bogdanov_takens_bifurcation, a, b, N, y0(i,3,:))
    y0(i,4,:) = [5.0, 10*real(i)/Mx-5]
    y(i,4,:,:) = AB5(bogdanov_takens_bifurcation, a, b, N, y0(i,4,:))
end do


do i=1,N
    t(i) = a + (b-a)/(N-1)*(i-1)
end do

! call PGENV(-5.0, 5.0, -5.0, 5.0, 0, 1)
! call PGLAB('y1', 'y2', 'Bogdanov–Takens bifurcation')

! call PGSCI(2)
sum_tmp = 0.0
do i = 1, Mx
    do j = 1, My
        ! call PGLINE(N,y(i,j,1,:),y(i,j,2,:))
        do k = 1, ubound(y, 4)
            if (.not. ieee_is_nan(y(i, j, 2, k)) .and. ieee_is_finite(y(i, j, 2, k))) then
                sum_tmp = sum_tmp + y(i, j, 2, k)
            end if
            if (.not. ieee_is_nan(y(i, j, 1, k)) .and. ieee_is_finite(y(i, j, 1, k))) then
                sum_tmp = sum_tmp + y(i, j, 1, k)
            end if
        end do
    end do
end do

print *, sum_tmp
if ( abs(sum_tmp - 7.64239229E+36) > 1e-8) error stop
print *, sum(t)
if ( abs(sum(t) - 25000.0039) > 1e-8) error stop
! call PGEND
end
