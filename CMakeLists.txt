cmake_minimum_required(VERSION 3.20)

project(FortranSimpleModel
  VERSION 0.1.0
  LANGUAGES Fortran C CXX # Note: C and CXX are not optional here!
  )

option(NO_FETCHCONTENT "Prevent automatic download of Fortran API" OFF)

find_package(fdpfort QUIET)
if(NOT fdpfort_FOUND AND NOT NO_FETCHCONTENT)
  if(NOT DEFINED(FDPFORTREF))
    set(FDPFORTREF "heads/main")
  endif()
  include(FetchContent)
  FetchContent_Declare(
    fdpfort
    URL "https://github.com/FAIRDataPipeline/FortranDataPipeline/archive/refs/${FDPFORTREF}.zip"
    )
  FetchContent_MakeAvailable(fdpfort)
endif()

set(SOURCES
  src/seirs.f90
  src/seirs_fair.f90
  src/fortran_simple_model.f90
  )

add_executable(fortran_simple_model ${SOURCES})
target_link_libraries(fortran_simple_model PRIVATE fdpfort::fdpfort)
set_target_properties(
    fortran_simple_model
  PROPERTIES
    Fortran_FORMAT FREE
    Fortran_MODULE_DIRECTORY "${PROJECT_BINARY_DIR}/mod"
    RUNTIME_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}/bin"
  )
