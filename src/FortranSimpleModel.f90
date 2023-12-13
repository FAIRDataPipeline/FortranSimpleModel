module fdp_fortran_simple_model

  use, intrinsic :: iso_fortran_env, only: dp => real64, stderr => error_unit
  implicit none
  private

  public :: t_seirs

  type, private :: t_seirs
    integer :: timesteps
    real(dp) :: years
    real(dp) :: alpha
    real(dp) :: beta
    real(dp) :: inv_gamma
    real(dp) :: inv_omega
    real(dp) :: inv_mu
    real(dp) :: inv_sigma
    real(dp), allocatable, dimension(:) :: time
    real(dp), allocatable, dimension(:) :: s
    real(dp), allocatable, dimension(:) :: e
    real(dp), allocatable, dimension(:) :: i
    real(dp), allocatable, dimension(:) :: r

    contains

      procedure, nopass :: init => t_seirs_init
      procedure, nopass :: from_file => t_seirs_from_file
      !procedure :: seirs_solve
      !procedure :: seirs_write

  end type t_seirs

contains

  ! Set up SEIRS type
  type(t_seirs) pure function t_seirs_init(timesteps, years, alpha, beta, &
      inv_gamma, inv_omega, inv_mu, inv_sigma, s, e, i, r) &
      result(self)
    integer, intent(in) :: timesteps
    real(dp), intent(in) :: years
    real(dp), intent(in) :: alpha
    real(dp), intent(in) :: beta
    real(dp), intent(in) :: inv_gamma
    real(dp), intent(in) :: inv_omega
    real(dp), intent(in) :: inv_mu
    real(dp), intent(in) :: inv_sigma
    real(dp), intent(in) :: s
    real(dp), intent(in) :: e
    real(dp), intent(in) :: i
    real(dp), intent(in) :: r
    integer :: j

    self%timesteps = timesteps
    self%years = years
    self%alpha = alpha
    self%beta = beta
    self%inv_gamma = inv_gamma
    self%inv_omega = inv_omega
    self%inv_mu = inv_mu
    self%inv_sigma = inv_sigma
    self%time = [(j * (years / timesteps), j=0, timesteps)]
    self%s = [(0.0_dp, j=0, timesteps)]
    self%e = [(0.0_dp, j=0, timesteps)]
    self%i = [(0.0_dp, j=0, timesteps)]
    self%r = [(0.0_dp, j=0, timesteps)]
    self%s(0) = s
    self%e(0) = e
    self%i(0) = i
    self%r(0) = r
  end function t_seirs_init

  ! Set up SEIRS type from input file
  type(t_seirs) function t_seirs_from_file(filename) result(self)
    character(*), intent(in) :: filename
    integer :: file_unit, ios
    logical :: file_exists
    character(64) :: key
    real(dp) :: val
    real(dp) :: alpha
    real(dp) :: beta
    real(dp) :: inv_gamma
    real(dp) :: inv_omega
    real(dp) :: inv_mu
    real(dp) :: inv_sigma

    inquire(file=filename, exist=file_exists)
    if(.not. file_exists) then
      write(stderr,*) "File ", filename, " not found"
      call exit(1)
    end if

    open(newunit=file_unit, file=filename)

    ! skip first line (contains headings)
    read(file_unit, "(A)") key

    ! read each line in sequence, assign if it matches a known name
    do
      read(file_unit, *, iostat=ios) key, val
      if(ios /= 0) exit
      if(key == "alpha") then
        alpha = val
      else if(key == "beta") then
        beta = val
      else if(key == "inv_gamma") then
        inv_gamma = val
      else if(key == "inv_omega") then
        inv_omega = val
      else if(key == "inv_mu") then
        inv_mu = val
      else if(key == "inv_sigma") then
        inv_sigma = val
      end if
    end do

    self = self%init(timesteps=10, years=5.0_dp, alpha=alpha, beta=beta, &
      inv_gamma=inv_gamma, inv_omega=inv_omega, inv_mu=inv_mu, &
      inv_sigma=inv_sigma, s=0.999_dp, e=0.001_dp, I=0.0_dp, R=0.0_dp)
  end function t_seirs_from_file

end module fdp_fortran_simple_model


program main

  use, intrinsic :: iso_fortran_env, only: dp => real64
  use fdp_fortran_simple_model
  implicit none

  type(t_seirs) :: seirs
  seirs = seirs%from_file("data/local_data.csv")

  write(*,*) seirs%years
  write(*,*) seirs%alpha
  write(*,*) seirs%beta
  write(*,*) seirs%inv_gamma
  write(*,*) seirs%inv_omega
  write(*,*) seirs%inv_mu
  write(*,*) seirs%inv_sigma
  write(*,*) seirs%time
  write(*,*) seirs%s
  write(*,*) seirs%e
  write(*,*) seirs%i
  write(*,*) seirs%r

end program main
