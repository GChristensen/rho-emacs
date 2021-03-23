cmake_minimum_required(VERSION 3.17)
project(rho)
set(CMAKE_VERBOSE_MAKEFILE ON)
set(CMAKE_BUILD_TYPE Release)
set(CMAKE_CXX_STANDARD 20)

add_definitions(-DUNICODE)
add_definitions(-D_UNICODE)


add_link_options(-municode -mwindows)

# options to build rhoc
#add_definitions(-DRHOC)
#add_link_options(-municode -mconsole -mwindows)

add_executable(rho main.cpp strutils.cpp strutils.h)

target_link_options(rho PUBLIC -static -static-libgcc -static-libstdc++)


