program fdp_simple_model

  use, intrinsic :: iso_fortran_env, only: dp => real64
  use fdp_seirs
  implicit none

  type(t_seirs) :: seirs
  call seirs%from_file("data/local_data.csv")
  call seirs%solve()
  call seirs%write("results.csv")

end program fdp_simple_model
