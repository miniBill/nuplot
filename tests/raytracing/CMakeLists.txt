cmake_minimum_required(VERSION 3.5)

project(raytracing_test LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
# cmake_policy(SET CMP0065 NEW)
# set(CMAKE_VERBOSE_MAKEFILE on)

add_executable(raytracing_test main.cpp)

target_compile_options(raytracing_test PRIVATE -Wall -fdata-sections -ffunction-sections -Wl,--gc-section)
target_link_options(raytracing_test PRIVATE -Wl,--gc-section)
