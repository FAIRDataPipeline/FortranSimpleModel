module fdp_seirs_fair
  use, intrinsic :: iso_fortran_env, only: dp => real64, stderr => error_unit
  use, intrinsic :: iso_c_binding, only: c_int
  use fdp_seirs, only: t_seirs
  use fairdatapipeline, only: FdpDataPipeline, fdp_log, FDP_LOG_INFO, FDP_LOG_ERROR
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

    character(*), parameter :: product_in = "SEIRS_model/parameters"
    character(*), parameter :: product_out = "SEIRS_model/results/model_output/fortran"
    
    call get_environment_variable("FDP_LOCAL_TOKEN", token)
    if(token == "") then
      call fdp_log(FDP_LOG_ERROR, "Could not find env var FDP_LOCAL_TOKEN")
      call exit(1)
    end if

    call get_environment_variable("FDP_CONFIG_DIR", config_dir)
    if(config_dir == "") then
      call fdp_log(FDP_LOG_ERROR, "Could not find env var FDP_CONFIG_DIR")
      call exit(1)
    end if

    config_path = trim(config_dir) // "/config.yaml"
    script_path = trim(config_dir) // "/script.sh"

    call fdp_log(FDP_LOG_INFO, "Config dir: " // trim(config_dir))
    call fdp_log(FDP_LOG_INFO, "Config path: " // trim(config_path))
    call fdp_log(FDP_LOG_INFO, "Script path: " // trim(script_path))

    call datapipeline%init(config_path, script_path, token, err=err)
    if(err /= 0) then
      write(err_log, "(A, I1)") "fdp_init failed, error code ", err
      call fdp_log(FDP_LOG_ERROR, err_log)
      call exit(1)
    end if

    input_path = datapipeline%link_read(product_in, err=err)
    if(err /= 0) then
      write(err_log, "(A, I1)") "fdp_link_read failed, error code ", err
      call fdp_log(FDP_LOG_ERROR, err_log)
      call exit(1)
    end if

    output_path = datapipeline%link_write(product_out, err=err)
    if(err /= 0) then
      write(err_log, "(A, I1)") "fdp_link_write failed, error code ", err
      call fdp_log(FDP_LOG_ERROR, err_log)
      call exit(1)
    end if
    
    call fdp_log(FDP_LOG_INFO, "Initialising SEIRS model")
    call seirs%from_file(trim(input_path))

    call fdp_log(FDP_LOG_INFO, "Solving SEIRS model")
    call seirs%solve()

    call fdp_log(FDP_LOG_INFO, "Writing SEIRS model to file")
    call seirs%write(trim(output_path))
    call fdp_log(FDP_LOG_INFO, "Written to file " // trim(output_path))

    call datapipeline%finalise()
  end subroutine run_fair

end module fdp_seirs_fair
