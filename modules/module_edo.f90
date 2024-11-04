!-----------------------------------------------------------------------
! module to solve Ordinary differential equation using:
!   1. Monostep
!       1.1. Euler
!           1.1.1 Explicit
!           1.1.2 Implicit (TODO)
!       1.2. Runge-Kutta
!           1.2.1.  fourth-order method (classical)
!   2. Multistep
!       2.1 Adams-Bashforth
!       2.2 Adams-Moulton   (TODO)
!       2.3 Nyström         (TODO)
!       2.4 Mile-Simpson    (TODO)
!
!   y' = f(x)       x in [a, b];   y in R**m
!   y(0) = eta      eta in R**m
!
! Licensing: This code is distributed under the GNU GPL license.
! Author: Carlos Planelles Alemany, planelles20(at)gmail(dot)com
!-----------------------------------------------------------------------

module module_edo

use module_no_linear_equations
implicit none

contains

    function odef1(x, y)
        real, allocatable :: odef1(:)
        real, intent(in) :: x, y(:)

        allocate(odef1(3))

        odef1(1) =  x + y(1)
        odef1(2) = -y(3) - y(2) - x
        odef1(3) =  x - y(1) + y(3)
    end function

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Euler Explicit
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    function EE(f, a, b, N, y0)
        real, allocatable :: EE(:,:)
        interface
            function f(x, y)
                real, allocatable :: f(:)
                real, intent(in) :: x, y(:)
            end function
        end interface
        real, intent(in) :: a, b
        integer, intent(in) :: N
        real, intent(in) :: y0(:)
        real :: xn, step
        integer :: i
        real, allocatable :: yn(:)

        allocate(EE(size(y0),N))
        step = (b-a)/(N-1)
        EE(:,1) = y0

        do i = 2, N
            xn = a+(i-2)*step
            yn = EE(:,i-1)
            EE(:,i) = yn + step*(f(xn, yn))
        end do
    end function

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Euler implicit (TODO)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!    function EI(f, a, b, N, y0)
!        real, allocatable :: EI(:,:)
!        interface
!            function f(x, y)
!                real, allocatable :: f(:)
!                real, intent(in) :: x, y(:)
!        end interface
!            end function
!        real, intent(in) :: a, b
!        real :: xn, step, eps
!        integer :: i
!        real, allocatable :: yn(:)
!        logical :: ok
!
!        allocate(EI(size(y0),N))
!        eps = 1e-5
!        step = (b-a)/(N-1)
!        EI(:,1) = y0
!
!        function fun(f, x, yans)
!            real, allocatable :: fun(:)
!            interface
!                function f(x, y)
!                    real, allocatable :: f(:)
!                    real, intent(in) :: x, y(:)
!                end function
!            end interface
!            real, intent(in) :: x, yans(:)
!            real :: y(:)
!
!            allocate(fun(size(y)))
!
!            y(:) = yans + step*f(x,yans)
!
!            fun(:) = yans + step*f(x,yans)
!
!        end function
!
!        do i = 2, N
!            xn = a+i*step
!            yn = EI(:,i-1) + step*(f(xn, yn))
!            !call fixPoint(fun, yn, ite, eps, ok)
!            EI(:,i) = yn
!        end do
!    end function

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Runge-Kutta fourth-order method (classical)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    function RK4(f, a, b, N, y0)
        real, allocatable :: RK4(:,:)
        interface
            function f(x, y)
                real, allocatable :: f(:)
                real, intent(in) :: x, y(:)
            end function
        end interface
        real, intent(in) :: a, b
        integer, intent(in) :: N
        real, intent(in) :: y0(:)
        real :: xn, step
        integer :: i
        real, allocatable :: yn(:), k1(:), k2(:), k3(:), k4(:)

        allocate(RK4(size(y0),N))
        allocate(k1(size(y0)), k2(size(y0)), k3(size(y0)), k4(size(y0)))
        step = (b-a)/(N-1)

        yn = y0
        RK4(:,1) = y0
        do i = 2, N
            xn = a+(i-2)*step
            k1 = step*f(xn, yn)
            k2 = step*f(xn+step/2.0, yn+k1/2.0)
            k3 = step*f(xn+step/2.0, yn+k2/2.0)
            k4 = step*f(xn+step, yn + k3)
            RK4(:,i) = yn + k1/6.0 + k2/3.0 + k3/3.0 + k4/6.0
            yn = RK4(:,i)
        end do
    end function

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Adams–Bashforth two steps
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    function AB2(a, b, N, y0)
        real, allocatable :: AB2(:,:)
        real, intent(in) :: a, b
        integer, intent(in) :: N
        real, intent(in) :: y0(:)
        real :: xn0, xn1, step
        real, allocatable :: yn0(:), yn1(:)
        integer :: i

        allocate(AB2(size(y0),N))
        step = (b-a)/(N-1)

        AB2(:,1) = y0
        !startup (Euler Explicit)
        AB2(:,2) = y0 + step*odef1(a, y0)

        do i = 3, N
            xn0 = a+(i-2)*step
            xn1 = a+(i-1)*step

            yn0 = AB2(:,i-2)
            yn1 = AB2(:,i-1)

            AB2(:,i) = AB2(:,i-1) + step*(3.0/2.0*odef1(xn1,yn1)-0.5*odef1(xn0,yn0))
        end do
    end function

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Adams–Bashforth five steps
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    function AB5(a, b, N, y0)
        real, allocatable :: AB5(:,:)
        real, intent(in) :: a, b
        integer, intent(in) :: N
        real, intent(in) :: y0(:)
        real :: step, baux, xn1, xn2, xn3, xn4, xn5
        real, allocatable :: yn1(:), yn2(:), yn3(:), yn4(:), yn5(:)
        integer :: i

        allocate(AB5(size(y0),N))
        step = (b-a)/(N-1)

        AB5(:,1) = y0
        !startup (Runge-Kutta 4th)
        baux = a+step*4
        AB5(:,2:4) = RK4(odef1, a, baux, 4, y0)

        do i = 5, N
            xn1 = a+(i-5)*step
            xn2 = a+(i-4)*step
            xn3 = a+(i-3)*step
            xn4 = a+(i-2)*step
            xn5 = a+(i-1)*step

            yn1 = AB5(:,i-5)
            yn2 = AB5(:,i-4)
            yn3 = AB5(:,i-3)
            yn4 = AB5(:,i-2)
            yn5 = AB5(:,i-1)

            AB5(:,i) = yn5 + step*(1901.0/720.0*odef1(xn5,yn5)-1387.0/360.0*odef1(xn4,yn4)+&
                       &109.0/30.0*odef1(xn3,yn3)-637.0/360.0*odef1(xn2,yn2)+251.0/720.0*odef1(xn1,yn1))
        end do
    end function

end module
