cmake_minimum_required(VERSION 3.19)
project(rho LANGUAGES CXX RC)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release)
endif()

# Prefer the UCRT runtime (built into Windows 10/11) when the toolchain supports
# it; a fully static executable then needs no MinGW DLLs. Some MinGW builds ship
# a libwinpthread.a that does not link against msvcrt, so the probe also checks
# that a static C++ program links.
include(CheckCXXSourceCompiles)
set(CMAKE_REQUIRED_FLAGS "-mcrtdll=ucrt")
set(CMAKE_REQUIRED_LINK_OPTIONS "-static")
check_cxx_source_compiles("#include <iostream>
int main(){std::cout<<1;}" RHO_HAVE_UCRT)
unset(CMAKE_REQUIRED_FLAGS)
unset(CMAKE_REQUIRED_LINK_OPTIONS)
if(RHO_HAVE_UCRT)
    add_compile_options(-mcrtdll=ucrt)
    add_link_options(-mcrtdll=ucrt)
endif()

set(RHO_SOURCES main.cpp args.cpp args.h config.cpp config.h server.cpp server.h winutil.cpp winutil.h)

function(rho_target name subsystem)
    add_executable(${name} ${RHO_SOURCES} rho.rc)
    target_compile_definitions(${name} PRIVATE UNICODE _UNICODE WIN32_LEAN_AND_MEAN NOMINMAX)
    target_compile_options(${name} PRIVATE -Wall -Wextra -Wpedantic -Wshadow -Wconversion)
    target_link_options(${name} PRIVATE -municode ${subsystem} -static)
    target_link_libraries(${name} PRIVATE ws2_32 shell32 ole32 uuid user32)
    set_source_files_properties(rho.rc PROPERTIES OBJECT_DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/rho.manifest")
    if(CMAKE_BUILD_TYPE STREQUAL "Release")
        target_link_options(${name} PRIVATE -s)
    endif()
endfunction()

# rho.exe  - GUI launcher (reads rho.conf)
rho_target(rho -mwindows)

# rhoc.exe - console launcher (reads rhoc.conf)
rho_target(rhoc -mconsole)
target_compile_definitions(rhoc PRIVATE RHOC)

# Unit tests of the pure helpers (not built by default into the installer).
option(RHO_BUILD_TESTS "Build launcher tests" OFF)
if(RHO_BUILD_TESTS)
    enable_testing()
    add_executable(rho_tests tests/tests.cpp args.cpp config.cpp server.cpp winutil.cpp)
    target_compile_definitions(rho_tests PRIVATE UNICODE _UNICODE WIN32_LEAN_AND_MEAN NOMINMAX RHOC)
    target_link_options(rho_tests PRIVATE -municode -mconsole -static)
    target_link_libraries(rho_tests PRIVATE ws2_32 shell32 ole32 uuid user32)
    add_test(NAME rho_tests COMMAND rho_tests)
endif()
