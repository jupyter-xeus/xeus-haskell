include(ExternalProject)

function(fetch_and_build_microhs MICROHS_BIN MICROHS_SRC_DIR)
    set(MICROHS_URL "https://github.com/augustss/MicroHs/archive/refs/tags/${XEUS_HASKELL_MICROHS_VERSION}.tar.gz")
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

    if(EMSCRIPTEN)
      set(HOST_ENV "EMSCRIPTEN=1" "CC=gcc" "CXX=g++" "LD=ld")
    else()
      set(HOST_ENV "EMSCRIPTEN=0")
    endif()

    ExternalProject_Add(MicroHsProject
        URL ${MICROHS_URL}
        DOWNLOAD_EXTRACT_TIMESTAMP TRUE
        BUILD_IN_SOURCE TRUE
        PREFIX ${MICROHS_PREFIX}
        CONFIGURE_COMMAND ""
        BUILD_COMMAND ${CMAKE_COMMAND} -E env ${HOST_ENV} ${MICROHS_MAKE} -f ${MICROHS_MAKEFILE}
        INSTALL_COMMAND ""
    )

    ExternalProject_Get_Property(MicroHsProject SOURCE_DIR)
    set(${MICROHS_SRC_DIR} ${SOURCE_DIR} PARENT_SCOPE)
    set(${MICROHS_BIN} "${SOURCE_DIR}/bin/mhs${MICROHS_SUFFIX}" PARENT_SCOPE)
endfunction()

function(build_and_install_libmhs MICROHS_BIN MICROHS_SRC_DIR)
    set(OUTPUT_HEADER "${CMAKE_CURRENT_BINARY_DIR}/Repl_stub.h")
    set(REPL_C "${CMAKE_CURRENT_BINARY_DIR}/Repl.c")
    set(EVAL_C "${MICROHS_SRC_DIR}/src/runtime/eval.c")
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

    add_library(repl_obj OBJECT ${REPL_C})
    add_dependencies(repl_obj MicroHsProject)
    target_include_directories(repl_obj PRIVATE
      ${MICROHS_SRC_DIR}/src/runtime
      ${MICROHS_OS_RUNTIME_DIR}
      ${CMAKE_BINARY_DIR}
    )
    target_compile_definitions(repl_obj PRIVATE __MHS__)
    if(EMSCRIPTEN)
        target_compile_options(repl_obj PRIVATE -fPIC)
    endif()

    add_library(eval_obj OBJECT ${EVAL_C})
    set_source_files_properties(${EVAL_C} PROPERTIES GENERATED TRUE)
    add_dependencies(eval_obj MicroHsProject)
    target_include_directories(eval_obj PRIVATE
      ${MICROHS_SRC_DIR}/src/runtime
      ${MICROHS_OS_RUNTIME_DIR}
      ${CMAKE_BINARY_DIR}
    )
    target_compile_definitions(eval_obj PRIVATE __MHS__)
    if(EMSCRIPTEN)
        target_compile_options(eval_obj PRIVATE -fPIC)
    endif()

    add_library(mhs_obj STATIC
        $<TARGET_OBJECTS:repl_obj>
        $<TARGET_OBJECTS:eval_obj>
    )
    add_dependencies(mhs_obj MicroHsProject)

    set(MHS_INCLUDE_DIR ${CMAKE_CURRENT_BINARY_DIR} PARENT_SCOPE)
    set(MHS_OBJ mhs_obj PARENT_SCOPE)
endfunction()
