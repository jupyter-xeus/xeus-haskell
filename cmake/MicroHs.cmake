include(ExternalProject)

function(fetch_and_build_microhs MICROHS_BIN MICROHS_SRC_DIR)
    set(MICROHS_VERSION "v0.14.21.0")
    set(MICROHS_URL "https://github.com/augustss/MicroHs/archive/refs/tags/${MICROHS_VERSION}.tar.gz")
    set(MICROHS_PREFIX "${CMAKE_BINARY_DIR}/microhs")
    if(WIN32)
      set(MICROHS_MAKE "nmake")
      set(MICROHS_MAKEFILE "Makefile.windows")
      set(MICROHS_SUFFIX ".exe")
    else()
      set(MICROHS_MAKE "make")
      set(MICROHS_MAKEFILE "Makefile")
      set(MICROHS_SUFFIX "")
    endif()

    ExternalProject_Add(MicroHsProject
        URL ${MICROHS_URL}
        DOWNLOAD_EXTRACT_TIMESTAMP TRUE
        BUILD_IN_SOURCE TRUE
        PREFIX ${MICROHS_PREFIX}
        CONFIGURE_COMMAND ""
        BUILD_COMMAND ${MICROHS_MAKE} -f ${MICROHS_MAKEFILE}
        INSTALL_COMMAND ""
    )

    ExternalProject_Get_Property(MicroHsProject SOURCE_DIR)
    set(${MICROHS_SRC_DIR} ${SOURCE_DIR} PARENT_SCOPE)
    set(${MICROHS_BIN} "${SOURCE_DIR}/bin/mhs${MICROHS_SUFFIX}" PARENT_SCOPE)
endfunction()

function(build_and_install_libmhs MICROHS_BIN MICROHS_SRC_DIR)
    set(OUTPUT_HEADER "${CMAKE_CURRENT_BINARY_DIR}/Repl_stub.h")
    set(REPL_C "${CMAKE_CURRENT_BINARY_DIR}/Repl.c")
    set(REPL_O "${CMAKE_CURRENT_BINARY_DIR}/Repl.o")
    set(EVAL_O "${CMAKE_CURRENT_BINARY_DIR}/eval.o")

    if(WIN32)
      set(MICROHS_OS_RUNTIME_DIR "${MICROHS_SRC_DIR}/src/runtime/windows")
    else()
      set(MICROHS_OS_RUNTIME_DIR "${MICROHS_SRC_DIR}/src/runtime/unix")
    endif()

    add_custom_command(
        OUTPUT ${REPL_C} ${OUTPUT_HEADER}
        COMMAND ${CMAKE_COMMAND} -E env
                "MHSDIR=${MICROHS_SRC_DIR}"
                ${MICROHS_BIN}
                -c
                -i${MICROHS_SRC_DIR}/mhs
                -i${MICROHS_SRC_DIR}/src
                -i${MICROHS_SRC_DIR}/lib
                -i${MICROHS_SRC_DIR}/paths
                -i${CMAKE_CURRENT_SOURCE_DIR}/src
                Repl
                -o Repl.c
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        COMMENT "Generating Repl.c and Repl_stub.h using MicroHs"
        VERBATIM
    )

    add_custom_command(
        OUTPUT ${EVAL_O}
        DEPENDS ${REPL_C}
        COMMAND ${CMAKE_C_COMPILER} -w -Wall -O3 -c
                -I${MICROHS_SRC_DIR}/src/runtime
                -I${MICROHS_OS_RUNTIME_DIR}
                -D__MHS__
                ${MICROHS_SRC_DIR}/src/runtime/eval.c
                -o ${EVAL_O}
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        VERBATIM
    )

    add_custom_command(
        OUTPUT ${REPL_O}
        DEPENDS ${REPL_C}
        COMMAND ${CMAKE_C_COMPILER} -w -Wall -O3 -c
                -I${MICROHS_SRC_DIR}/src/runtime
                -I${MICROHS_OS_RUNTIME_DIR}
                -D__MHS__
                ${REPL_C}
                -o ${REPL_O}
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        VERBATIM
    )

    add_library(mhs_obj STATIC ${REPL_O} ${EVAL_O})
    add_dependencies(mhs_obj MicroHsProject)

    set(MHS_INCLUDE_DIR ${CMAKE_CURRENT_BINARY_DIR} PARENT_SCOPE)
    set(MHS_OBJ mhs_obj PARENT_SCOPE)
    set_source_files_properties(${REPL_O} ${EVAL_O} PROPERTIES GENERATED TRUE)
    set_target_properties(mhs_obj PROPERTIES LINKER_LANGUAGE C)
endfunction()
