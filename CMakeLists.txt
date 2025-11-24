#############################################################################
#Copyright (c) 2025,
#
#Distributed under the terms of the Apache Software License 2.0.
#
#The full license is in the file LICENSE, distributed with this software.
#############################################################################

cmake_minimum_required(VERSION 3.10.0)
project(xeus-haskell)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

set(CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake;${CMAKE_MODULE_PATH}")

set(XEUS_HASKELL_INCLUDE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/include)

if(NOT DEFINED XEUS_HASKELL_MICROHS_VERSION)
    set(XEUS_HASKELL_MICROHS_VERSION "v0.14.23.1" CACHE STRING "MicroHs tag to fetch")
endif()

# Versionning
# ===========

file(STRINGS "${XEUS_HASKELL_INCLUDE_DIR}/xeus-haskell/xeus_haskell_config.hpp" version_defines
     REGEX "#define XEUS_HASKELL_VERSION_(MAJOR|MINOR|PATCH)")
foreach (ver ${version_defines})
    if (ver MATCHES "#define XEUS_HASKELL_VERSION_(MAJOR|MINOR|PATCH) +([^ ]+)$")
        set(XEUS_HASKELL_VERSION_${CMAKE_MATCH_1} "${CMAKE_MATCH_2}" CACHE INTERNAL "")
    endif ()
endforeach ()
set(${PROJECT_NAME}_VERSION
    ${XEUS_HASKELL_VERSION_MAJOR}.${XEUS_HASKELL_VERSION_MINOR}.${XEUS_HASKELL_VERSION_PATCH})
message(STATUS "Building xeus-haskell v${${PROJECT_NAME}_VERSION}")

# Configuration
# =============

include(GNUInstallDirs)

if (NOT DEFINED XEUS_HASKELL_KERNELSPEC_PATH)
    set(XEUS_HASKELL_KERNELSPEC_PATH "${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_BINDIR}/")
endif ()

configure_file (
    "${CMAKE_CURRENT_SOURCE_DIR}/share/jupyter/kernels/xhaskell/kernel.json.in"
    "${CMAKE_CURRENT_SOURCE_DIR}/share/jupyter/kernels/xhaskell/kernel.json"
)

# Build options
# =============

option(XEUS_HASKELL_BUILD_STATIC "Build xeus-haskell static library" ON)
# CHANGED: default OFF instead of ON
OPTION(XEUS_HASKELL_BUILD_SHARED "Split xhaskell build into executable and library" OFF)
OPTION(XEUS_HASKELL_BUILD_EXECUTABLE "Build the xhaskell executable" ON)

OPTION(XEUS_HASKELL_USE_SHARED_XEUS "Link xhaskell  with the xeus shared library (instead of the static library)" ON)
# CHANGED: default OFF instead of ON, so xhaskell links against xeus-haskell-static
OPTION(XEUS_HASKELL_USE_SHARED_XEUS_HASKELL "Link xhaskell  with the xeus shared library (instead of the static library)" OFF)

if(EMSCRIPTEN)
    add_compile_definitions(XEUS_HASKELL_EMSCRIPTEN_WASM_BUILD)
    message("Build with emscripten")
    SET(XEUS_HASKELL_BUILD_STATIC ON)
    SET(XEUS_HASKELL_BUILD_SHARED OFF)
    SET(XEUS_HASKELL_BUILD_EXECUTABLE OFF)
    SET(XEUS_HASKELL_USE_SHARED_XEUS OFF)
    SET(XEUS_HASKELL_USE_SHARED_XEUS_HASKELL OFF)
    SET(XEUS_HASKELL_BUILD_TESTS OFF)
endif()

# Dependencies
# ============

set(xeus_REQUIRED_VERSION "5.0.0" CACHE STRING "Minimum xeus version required to build xeus-haskell")

find_package(xeus ${xeus_REQUIRED_VERSION} REQUIRED)

# Flags
# =====
include(CheckCXXCompilerFlag)

if (MSVC)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /wd4251 /wd4141")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /wd4018 /wd4267 /wd4715 /wd4146 /wd4129")
endif ()

if (CMAKE_CXX_COMPILER_ID MATCHES "Clang" OR CMAKE_CXX_COMPILER_ID MATCHES "GNU" OR CMAKE_CXX_COMPILER_ID MATCHES "Intel")
    if(NOT EMSCRIPTEN)
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wunused-parameter -Wextra -Wreorder")
    endif()

    CHECK_CXX_COMPILER_FLAG("-std=c++17" HAS_CPP_17_FLAG)
    if (HAS_CPP_17_FLAG)
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++17")
    else ()
        message(FATAL_ERROR "Unsupported compiler -- xeus requires C++17 support!")
    endif ()
endif ()

if(EMSCRIPTEN)
    # ENV (https://github.com/emscripten-core/emscripten/commit/6d9681ad04f60b41ef6345ab06c29bbc9eeb84e0)
    set(EMSCRIPTEN_FEATURES "${EMSCRIPTEN_FEATURES} -s \"EXTRA_EXPORTED_RUNTIME_METHODS=['ENV']\"")
endif()

# Source files
# ============

set(XEUS_HASKELL_HEADERS
    include/xeus-haskell/xeus_haskell_config.hpp
    include/xeus-haskell/xinterpreter.hpp
)

set(XEUS_HASKELL_SRC
    src/xinterpreter.cpp
)

set(XEUS_HASKELL_MAIN_SRC
    src/main.cpp
)

# MicroHs / mhs_repl integration
# ==============================

include(MicroHs)
fetch_and_build_microhs(MICROHS_BIN MICROHS_SRC_DIR)
build_and_install_libmhs(${MICROHS_BIN} ${MICROHS_SRC_DIR})

add_library(mhs_repl_lib STATIC
    src/mhs_repl.cpp
)

add_dependencies(mhs_repl_lib mhs_obj)

target_include_directories(mhs_repl_lib
    PUBLIC
        ${MHS_INCLUDE_DIR}
        ${XEUS_HASKELL_INCLUDE_DIR}
)

target_link_libraries(mhs_repl_lib
    PUBLIC ${MHS_OBJ}
)

if(EMSCRIPTEN)
    target_compile_options(mhs_repl_lib PRIVATE -fPIC)
endif()

# Targets and link - Macros
# =========================

include(CheckCXXCompilerFlag)

string(TOUPPER "${CMAKE_BUILD_TYPE}" U_CMAKE_BUILD_TYPE)

set(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib;${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}")

macro(xeus_haskell_set_common_options target_name)
    if (MSVC)
        target_compile_options(${target_name} PUBLIC /wd4251 /wd4141)
        target_compile_options(${target_name} PUBLIC /wd4018 /wd4267 /wd4715 /wd4146 /wd4129)
    endif ()

    if (CMAKE_CXX_COMPILER_ID MATCHES "Clang" OR
        CMAKE_CXX_COMPILER_ID MATCHES "GNU" OR
        CMAKE_CXX_COMPILER_ID MATCHES "Intel")

        target_compile_options(${target_name} PUBLIC -Wunused-parameter -Wextra -Wreorder)

        # C++17 flag
        CHECK_CXX_COMPILER_FLAG("-std=c++17" HAS_CPP_17_FLAG)
        if (HAS_CPP_17_FLAG)
            target_compile_features(${target_name} PRIVATE cxx_std_17)
        else ()
            message(FATAL_ERROR "Unsupported compiler -- xeus-haskell requires C++17 support!")
        endif ()
    endif ()

    if (APPLE)
        set_target_properties(${target_name} PROPERTIES
            MACOSX_RPATH ON
        )
    else ()
        set_target_properties(${target_name} PROPERTIES
            BUILD_WITH_INSTALL_RPATH 1
            SKIP_BUILD_RPATH FALSE
        )
    endif ()

    set_target_properties(${target_name} PROPERTIES
        INSTALL_RPATH_USE_LINK_PATH TRUE
    )
endmacro()

# Common macro kernels (xhaskell)
macro(xeus_haskell_set_kernel_options target_name)
    if (XEUS_HASKELL_USE_SHARED_XEUS_HASKELL)
        target_link_libraries(${target_name} PRIVATE xeus-haskell)
        if(CMAKE_DL_LIBS)
            target_link_libraries(${target_name} PRIVATE ${CMAKE_DL_LIBS} util)
        endif()
    else ()
        target_link_libraries(${target_name} PRIVATE xeus-haskell-static)
    endif()

    if(NOT EMSCRIPTEN)
        find_package(Threads)
        target_link_libraries(${target_name} PRIVATE ${CMAKE_THREAD_LIBS_INIT})
    endif()
endmacro()

# Common macro for shared and static library
macro(xeus_haskell_create_target target_name linkage output_name)
    string(TOUPPER "${linkage}" linkage_upper)

    if (NOT ${linkage_upper} MATCHES "^(SHARED|STATIC)$")
        message(FATAL_ERROR "Invalid library linkage: ${linkage}")
    endif ()

    add_library(${target_name} ${linkage_upper} ${XEUS_HASKELL_SRC} ${XEUS_HASKELL_HEADERS})
    xeus_haskell_set_common_options(${target_name})

    set_target_properties(${target_name} PROPERTIES
                          PUBLIC_HEADER "${XEUS_HASKELL_HEADERS}"
                          PREFIX ""
                          VERSION ${${PROJECT_NAME}_VERSION}
                          SOVERSION ${XEUS_HASKELL_VERSION_MAJOR}
                          OUTPUT_NAME "lib${output_name}")

    target_compile_definitions(${target_name}
                               PUBLIC
                               XEUS_HASKELL_EXPORTS
                               XEUS_HASKELL_MICROHS_VERSION="${XEUS_HASKELL_MICROHS_VERSION}")

    target_compile_features(${target_name} PRIVATE cxx_std_17)

    target_include_directories(${target_name}
                               PUBLIC
                               $<BUILD_INTERFACE:${XEUS_HASKELL_INCLUDE_DIR}>
                               $<INSTALL_INTERFACE:include>)

    if (XEUS_HASKELL_USE_SHARED_XEUS)
        set(XEUS_HASKELL_XEUS_TARGET xeus)
    else ()
        set(XEUS_HASKELL_XEUS_TARGET xeus-static)
    endif ()

    target_link_libraries(${target_name} PUBLIC ${XEUS_HASKELL_XEUS_TARGET})

    target_link_libraries(${target_name} PUBLIC mhs_repl_lib)

    if (WIN32 OR CYGWIN)
        #
    elseif (APPLE)
        target_link_libraries(${target_name} PRIVATE "-undefined dynamic_lookup")
    endif ()
    if(NOT EMSCRIPTEN)
        find_package(Threads) # TODO: add Threads as a dependence of xeus-static?
        target_link_libraries(${target_name} PRIVATE ${CMAKE_THREAD_LIBS_INIT})
    endif()
endmacro()

# xeus-haskell
# ===========

set(XEUS_HASKELL_TARGETS "")

if (XEUS_HASKELL_BUILD_SHARED)
    # Build libraries
    xeus_haskell_create_target(xeus-haskell SHARED xeus-haskell)
    list(APPEND XEUS_HASKELL_TARGETS xeus-haskell)
endif ()

if (XEUS_HASKELL_BUILD_STATIC)
    # On Windows, a static library should use a different output name
    # to avoid the conflict with the import library of a shared one.
    if (CMAKE_HOST_WIN32)
        xeus_haskell_create_target(xeus-haskell-static STATIC xeus-haskell-static)
    else ()
        xeus_haskell_create_target(xeus-haskell-static STATIC xeus-haskell)
    endif ()
    if(EMSCRIPTEN)
        target_compile_options(xeus-haskell-static PRIVATE -fPIC)
    endif()
    list(APPEND XEUS_HASKELL_TARGETS xeus-haskell-static)
endif ()

# xhaskell (native)
# =======
if (XEUS_HASKELL_BUILD_EXECUTABLE)
    find_package(xeus-zmq 1.0.2 REQUIRED)
    add_executable(xhaskell ${XEUS_HASKELL_MAIN_SRC})
    target_compile_features(xhaskell PRIVATE cxx_std_17)
    xeus_haskell_set_common_options(xhaskell)
    xeus_haskell_set_kernel_options(xhaskell)
    target_link_libraries(xhaskell PRIVATE xeus-zmq)
    target_link_libraries(xhaskell PRIVATE mhs_repl_lib)
endif()

# xhaskell (emscripten / wasm)
if(EMSCRIPTEN)
    include(WasmBuildOptions)
    find_package(xeus-lite REQUIRED)
    add_executable(xhaskell src/main_emscripten_kernel.cpp )
    target_link_libraries(xhaskell PRIVATE xeus-lite)
    target_compile_options(xhaskell PRIVATE -fPIC)
    # mhs_repl_lib is already pulled in transitively via xeus-haskell-static,
    # so linking it explicitly here would add it twice and cause duplicate symbols
    # target_link_libraries(xhaskell PRIVATE mhs_repl_lib)
    XEUS_HASKELL_set_kernel_options(xhaskell)
    xeus_wasm_compile_options(xhaskell)
    xeus_wasm_link_options(xhaskell "web,worker")
    target_link_options(xhaskell
      PUBLIC --bind
      PUBLIC "SHELL: -sWASMFS"
      PUBLIC "SHELL: --pre-js ${CMAKE_CURRENT_SOURCE_DIR}/src/pre.js"
      PUBLIC "SHELL: --post-js ${CMAKE_CURRENT_SOURCE_DIR}/src/post.js"
    )
endif()

# Installation
# ============
include(CMakePackageConfigHelpers)

set(XEUS_HASKELL_CMAKECONFIG_INSTALL_DIR "${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME}" CACHE STRING "install path for xeus-haskellConfig.cmake")

# Install xeus-haskell and xeus-haskell-static
if (XEUS_HASKELL_BUILD_SHARED)
    install(TARGETS ${XEUS_HASKELL_TARGETS}
            EXPORT ${PROJECT_NAME}-targets
            ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
            LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
            RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
            PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/xeus-haskell)

    # Makes the project importable from the build directory
    export(EXPORT ${PROJECT_NAME}-targets
           FILE "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}Targets.cmake")
endif ()

# Install xhaskell
if (XEUS_HASKELL_BUILD_EXECUTABLE)
    install(TARGETS xhaskell
            RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
endif()

if (XEUS_HASKELL_BUILD_EXECUTABLE OR EMSCRIPTEN)
    # Configuration and data directories for jupyter and xeus-haskell
    set(XJUPYTER_DATA_DIR "share/jupyter"    CACHE STRING "Jupyter data directory")

    # Install xhaskell Jupyter kernelspec
    set(KERNELSPEC_DIR ${CMAKE_CURRENT_SOURCE_DIR}/share/jupyter/kernels)
    install(DIRECTORY ${KERNELSPEC_DIR}
            DESTINATION ${XJUPYTER_DATA_DIR}
            PATTERN "*.in" EXCLUDE)


    # Extra path for installing Jupyter kernelspec
    if (XEXTRA_JUPYTER_DATA_DIR)
        install(DIRECTORY ${KERNELSPEC_DIR}
                DESTINATION ${XEXTRA_JUPYTER_DATA_DIR}
                PATTERN "*.in" EXCLUDE)
    endif ()
endif ()

# Configure 'xeus-haskellConfig.cmake' for a build tree
set(XEUS_HASKELL_CONFIG_CODE "####### Expanded from \@XEUS_HASKELL_CONFIG_CODE\@ #######\n")
set(XEUS_HASKELL_CONFIG_CODE "${XEUS_HASKELL_CONFIG_CODE}set(CMAKE_MODULE_PATH \"${CMAKE_CURRENT_SOURCE_DIR}/cmake;\${CMAKE_MODULE_PATH}\")\n")
set(XEUS_HASKELL_CONFIG_CODE "${XEUS_HASKELL_CONFIG_CODE}##################################################")
configure_package_config_file(${PROJECT_NAME}Config.cmake.in
                              "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}Config.cmake"
                              INSTALL_DESTINATION ${PROJECT_BINARY_DIR})

# Configure 'xeus-haskellConfig.cmake.in for an install tree
set(XEUS_HASKELL_CONFIG_CODE "")
configure_package_config_file(${PROJECT_NAME}Config.cmake.in
                              "${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/${PROJECT_NAME}Config.cmake"
                              INSTALL_DESTINATION ${XEUS_HASKELL_CMAKECONFIG_INSTALL_DIR})

write_basic_package_version_file(${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}ConfigVersion.cmake
                                 VERSION ${${PROJECT_NAME}_VERSION}
                                 COMPATIBILITY AnyNewerVersion)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/${PROJECT_NAME}Config.cmake
              ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}ConfigVersion.cmake
              DESTINATION ${XEUS_HASKELL_CMAKECONFIG_INSTALL_DIR})

if (XEUS_HASKELL_BUILD_SHARED)
    install(EXPORT ${PROJECT_NAME}-targets
            FILE ${PROJECT_NAME}Targets.cmake
            DESTINATION ${XEUS_HASKELL_CMAKECONFIG_INSTALL_DIR})
endif ()

if(EMSCRIPTEN)
    install(TARGETS xhaskell
            ARCHIVE DESTINATION ${XEUS_HASKELL_CMAKECONFIG_INSTALL_DIR})

    install(FILES
            "$<TARGET_FILE_DIR:xhaskell>/xhaskell.js"
            "$<TARGET_FILE_DIR:xhaskell>/xhaskell.wasm"
            DESTINATION ${CMAKE_INSTALL_BINDIR})
endif ()

# MicroUnitTesting / test_mhs_repl
# ================================
if(NOT EMSCRIPTEN)
    include(MicroUnitTesting)
    enable_testing()
    fetch_ut()
    add_executable(test_mhs_repl test/test_mhs_repl.cpp)
    add_dependencies(test_mhs_repl mhs_repl_lib)
    target_include_directories(test_mhs_repl
        PUBLIC
            $<BUILD_INTERFACE:${XEUS_HASKELL_INCLUDE_DIR}>
            $<INSTALL_INTERFACE:include>
        PRIVATE
            ${MHS_INCLUDE_DIR})
    target_compile_definitions(test_mhs_repl PRIVATE MICROHS_RUNTIME_DIR="${MICROHS_SRC_DIR}")
    target_link_libraries(test_mhs_repl PRIVATE mhs_repl_lib Boost::ut)
    add_test(NAME test_mhs_repl COMMAND test_mhs_repl)
endif()

# XHaskell library
# ===================
install(
    DIRECTORY ${CMAKE_SOURCE_DIR}/src/XHaskell/
    DESTINATION usr/lib/haskell-packages/microhs/XHaskell
)

