module fdp_seirs_fair
  use, intrinsic :: iso_fortran_env, only: dp => real64, stderr => error_unit
  use, intrinsic :: iso_c_binding, only: c_int
  use fdp_seirs, only: t_seirs
  use fairdatapipeline
  implicit none
  private

  public :: run_fair

contains

  impure subroutine run_fair
    character(len=1024) :: token, config_dir, config_path, script_path
    character(len=4096) :: err_log
    character(:), allocatable :: input_path, output_path
    type(FdpDataPipeline) :: datapipeline
    type(t_seirs) :: seirs
    integer(kind=c_int) :: err
    
    call get_environment_variable("FDP_LOCAL_TOKEN", token)
    if(token == "") then
      err = fdp_log(FDP_LOG_ERROR, "Could not find env var FDP_LOCAL_TOKEN")
      call exit(1)
    end if

    call get_environment_variable("FDP_CONFIG_DIR", config_dir)
    if(config_dir == "") then
      err = fdp_log(FDP_LOG_ERROR, "Could not find env var FDP_CONFIG_DIR")
      call exit(1)
    end if

    config_path = trim(config_dir) // "/config.yaml"
    script_path = trim(config_dir) // "/script.sh"

    err = fdp_log(FDP_LOG_INFO, "Config dir: " // trim(config_dir))
    err = fdp_log(FDP_LOG_INFO, "Config path: " // trim(config_path))
    err = fdp_log(FDP_LOG_INFO, "Script path: " // trim(script_path))

    err = fdp_init(datapipeline, config_path, script_path, token)
    if(err /= 0) then
      write(err_log, "(A, I1)") "fdp_init failed, error code ", err
      err = fdp_log(FDP_LOG_ERROR, err_log)
      call exit(1)
    end if

    call fdp_link_read(datapipeline, "SEIRS_model/parameters", input_path, err)
    if(err /= 0) then
      write(err_log, "(A, I1)") "fdp_link_read failed, error code ", err
      err = fdp_log(FDP_LOG_ERROR, err_log)
      call exit(1)
    end if

    call fdp_link_write(datapipeline, "SEIRS_model/results/model_output/fortran", &
      output_path, err)
    if(err /= 0) then
      write(err_log, "(A, I1)") "fdp_link_write failed, error code ", err
      err = fdp_log(FDP_LOG_ERROR, err_log)
      call exit(1)
    end if
    
    err = fdp_log(FDP_LOG_INFO, "Initialising SEIRS model")
    call seirs%from_file(trim(input_path))

    err = fdp_log(FDP_LOG_INFO, "Solving SEIRS model")
    call seirs%solve()

    err = fdp_log(FDP_LOG_INFO, "Writing SEIRS model to file")
    call seirs%write(trim(output_path))
    err = fdp_log(FDP_LOG_INFO, "Written to file " // trim(output_path))

    err = fdp_finalise(datapipeline)
  end subroutine run_fair

end module fdp_seirs_fair
