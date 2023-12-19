module fdp_seirs

  use, intrinsic :: iso_fortran_env, only: dp => real64, stderr => error_unit
  implicit none
  private

  public :: t_seirs
  public :: run_local

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

      procedure :: init => t_seirs_init
      procedure :: from_file => t_seirs_from_file
      procedure :: solve => t_seirs_solve
      procedure :: write => t_seirs_write

  end type t_seirs

contains

  ! Set up SEIRS type
  pure subroutine t_seirs_init(self, timesteps, years, alpha, beta, &
      inv_gamma, inv_omega, inv_mu, inv_sigma, s, e, i, r)
    class(t_seirs), intent(inout) :: self
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
    allocate(self%time(0:timesteps))
    allocate(self%s(0:timesteps), self%e(0:timesteps), self%i(0:timesteps), &
      self%r(0:timesteps), source=0.0_dp)
    do j=0, timesteps
      self%time(j) = j * years / timesteps
    end do
    self%s(0) = s
    self%e(0) = e
    self%i(0) = i
    self%r(0) = r
  end subroutine t_seirs_init

  ! Set up SEIRS type from input file
  impure subroutine t_seirs_from_file(self, filename)
    class(t_seirs), intent(inout) :: self
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

    open(newunit=file_unit, file=filename, action="READ")

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

    close(file_unit)

    call self%init(timesteps=1000, years=5.0_dp, alpha=alpha, beta=beta, &
      inv_gamma=inv_gamma, inv_omega=inv_omega, inv_mu=inv_mu, &
      inv_sigma=inv_sigma, s=0.999_dp, e=0.001_dp, I=0.0_dp, R=0.0_dp)
  end subroutine t_seirs_from_file

  ! Solve SEIRS problem
  pure subroutine t_seirs_solve(self)
    class(t_seirs), intent(inout) :: self
    real(dp) :: dt, N, alpha, beta, gamma, omega, mu, sigma
    real(dp) :: birth, infection, lost_immunity, latency, recovery
    real(dp) :: death_s, death_e, death_i, death_r
    real(dp) :: rate_s, rate_e, rate_i, rate_r
    integer :: idx

    dt = 365.25_dp * self%years / self%timesteps
    alpha = dt * self%alpha
    beta = dt * self%beta
    gamma = dt / self%inv_gamma
    omega = dt / (self%inv_omega * 365.25_dp)
    mu = dt / (self%inv_mu * 365.25_dp)
    sigma = dt / self%inv_sigma

    do idx=0, self%timesteps-1
      N = self%s(idx) + self%e(idx) + self%i(idx) + self%r(idx)
      birth = N * mu
      infection = beta * self%i(idx) * self%s(idx) / N
      lost_immunity = omega * self%r(idx)
      death_s = mu * self%s(idx)
      death_e = mu * self%e(idx)
      death_i = (mu + alpha) * self%i(idx)
      death_r = mu * self%r(idx)
      latency = sigma * self%e(idx)
      recovery = gamma * self%i(idx)
      rate_s = birth - infection + lost_immunity - death_s
      rate_e = infection - latency - death_e
      rate_i = latency - recovery - death_i
      rate_r = recovery - lost_immunity - death_r
      self%s(idx+1) = self%s(idx) + rate_s
      self%e(idx+1) = self%e(idx) + rate_e
      self%i(idx+1) = self%i(idx) + rate_i
      self%r(idx+1) = self%r(idx) + rate_r
    end do
  end subroutine t_seirs_solve

  impure subroutine t_seirs_write(self, output)
    class(t_seirs), intent(in) :: self
    character(*), intent(in) :: output
    integer :: idx, file_unit

    open(newunit=file_unit, file=output, action="WRITE")
    write(file_unit, "(A)") "time,S,E,I,R"
    do idx=0, self%timesteps
      write(file_unit, "(*(G0.6,:,','))") self%time(idx), self%s(idx), &
        self%e(idx), self%i(idx), self%r(idx)
    end do
    close(file_unit)
  end subroutine t_seirs_write

  impure subroutine run_local(input_file)
    character(*), intent(in) :: input_file
    type(t_seirs) :: seirs

    call seirs%from_file(input_file)
    call seirs%solve()
    call seirs%write("data_store/fortran_simple_model.csv")
  end subroutine run_local

end module fdp_seirs
