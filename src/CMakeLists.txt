set(SOURCES
  fdp_seirs.f90
  fdp_simple_model.f90
  )

add_executable(simple_model.exe ${SOURCES})
set_target_properties(
    simple_model.exe
  PROPERTIES
    Fortran_FORMAT FREE
    Fortran_MODULE_DIRECTORY "${PROJECT_BINARY_DIR}/mod"
    RUNTIME_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}/bin"
  )
