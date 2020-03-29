! -*- coding: utf-8 -*-
program k_means
  use mod_force_raise
  use mod_sub
  implicit none

  real(8),allocatable :: xn(:, :)
  real(8),allocatable :: mu(:, :)
  integer :: i
  integer :: k
  integer :: n
  integer,allocatable :: numk(:)
  integer,allocatable :: gk(:)
  integer,allocatable :: tmp_gk(:)
  real(8),allocatable :: tmp_mu(:, :)
  integer :: seedsize
  integer,allocatable :: seed(:)

  ! this code is k==3 only

  n = 10
  k = 3
  n = n*k
  allocate(xn(2, n))
  allocate(mu(2, k))
  allocate(tmp_mu(2, k))
  allocate(gk(n))
  allocate(tmp_gk(n))
  allocate(numk(k))

  call random_seed(size=seedsize)
  allocate(seed(seedsize))
  call random_seed(get=seed)

  write(*,*) "seedsize", seedsize
  write(*,*) "seed", seed

  call random_number(xn)

  xn(1, n/3+1:2*n/3) = xn(1, n/3+1:2*n/3) + 1d0 ! k==3 only
  xn(1, 2*n/3+1:n) = xn(1, 2*n/3+1:n) + 2d0 ! k==3 only
  do i = 1, n
     write(*,*) 'i, xn(i)', i, xn(:, i)
  end do

  mu(:, 1:k) = xn(:, 1:k)
!  do j = 1, k
!     mu(:, j) = xn(:, j)
!!     mu(2, j) = xn(2, j)
!  end do

  tmp_gk = 0
  tmp_mu = 0d0
  do i = 1, 100
     write(*,*) 'loop', i, 'is started'
     call grouping(xn, mu, gk)
     call mean_group(xn, mu, gk, numk)
     if(sum(gk) .eq. sum(tmp_gk) .and. sum(mu) .eq. sum(tmp_mu)) exit
     tmp_gk = gk
     tmp_mu = mu
  end do

  do i = 1, n
     write(*,*) 'i, gk(i), xn(i)', i, gk(i), xn(:, i)
  end do

  do i = 1, k
     write(*,*) 'i, numk(i), mu(i)', i, numk(i), mu(:, i)
  end do

end program k_means
