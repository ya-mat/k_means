! -*- coding: utf-8 -*-
module mod_sub
  implicit none
contains
!--------------------------------------------
  function dist2(x, y) result(res)
    implicit none

    real(8), intent(in) :: x(:)
    real(8), intent(in) :: y(:)
    real(8) :: res

    res = (x(1) - y(1))**2d0 + (x(2) - y(2))**2d0

    return

  end function dist2
!--------------------------------------------
  subroutine grouping(xn, mu, gk)
    implicit none

    real(8),intent(in) :: xn(:, :)
    real(8),intent(in) :: mu(:, :)
    integer,intent(out) :: gk(:)
    real(8) :: now
    real(8) :: tmp
    integer :: i
    integer :: j

    do i = 1, size(xn(1, :))
       now = 0d0
       do j = 1, size(mu(1, :))
          tmp = dist2(xn(:, i), mu(:, j))
          if(now .eq. 0d0) then
             now = tmp
             gk(i) = 1
          elseif(tmp .le. now) then
             now = tmp
             gk(i) = j
          end if
       end do
    end do

  end subroutine grouping
!--------------------------------------------
  subroutine mean_group(xn, mu, gk, numk)
    implicit none

    real(8),intent(in) :: xn(:, :)
    real(8),intent(out) :: mu(:, :)
    integer,intent(in) :: gk(:)
    integer,intent(out) :: numk(:)
    real(8) :: now
    real(8) :: tmp
    integer :: i
    integer :: j

    numk = 0
    do j = 1, size(mu(1, :))
       mu(:, j) = (/0d0, 0d0/)
       do i = 1, size(xn(1, :))
          if(gk(i) .eq. j) then
             numk(j) = numk(j) + 1
             mu(:, j) = mu(:, j) + xn(:, i)
          end if
       end do
       mu(:, j) = mu(:, j)/numk(j)
    end do

  end subroutine mean_group
!--------------------------------------------
end module mod_sub
