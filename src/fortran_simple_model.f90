program fortran_simple_model
  use, intrinsic :: iso_fortran_env, only: dp => real64, stderr => error_unit
  use fdp_seirs, only: run_local
  use fdp_seirs_fair, only: run_fair
  implicit none

  integer :: arg_count
  character(len=1024) :: local_file

  arg_count = command_argument_count()
  if(arg_count == 0) then
    call run_fair()
  else if(arg_count == 1) then
    call get_command_argument(1, local_file)
    call run_local(trim(local_file))
  else
    write(stderr, *) "Usage:"
    write(stderr, *) "./fortran_simple_model : Run using FAIR CLI"
    write(stderr, *) "./fortran_simple_model initial_parameters_path : &
      &Run using provided data file"
    call exit(1)
  end if
end program fortran_simple_model
