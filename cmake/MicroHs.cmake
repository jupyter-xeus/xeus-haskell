include(ExternalProject)

function(fetch_and_build_microhs MICROHS_BIN MICROHS_SRC_DIR)
    set(MICROHS_VERSION "v0.14.21.0")
    set(MICROHS_URL "https://github.com/augustss/MicroHs/archive/refs/tags/${MICROHS_VERSION}.tar.gz")
    set(MICROHS_PREFIX "${CMAKE_BINARY_DIR}/microhs")

    ExternalProject_Add(MicroHsProject
        URL ${MICROHS_URL}
        PREFIX ${MICROHS_PREFIX}
        CONFIGURE_COMMAND ""  # skip configuration
        BUILD_COMMAND make -C <SOURCE_DIR> bin/mhs
        INSTALL_COMMAND ""    # skip installation
    )

    ExternalProject_Get_Property(MicroHsProject SOURCE_DIR)
    set(${MICROHS_SRC_DIR} ${SOURCE_DIR} PARENT_SCOPE)
    set(${MICROHS_BIN} "${SOURCE_DIR}/bin/mhs" PARENT_SCOPE)
endfunction()

include(GNUInstallDirs)

function(build_and_install_libmhs MICROHS_BIN MICROHS_SRC_DIR)
    set(OUTPUT_HEADER "${CMAKE_CURRENT_BINARY_DIR}/Repl_stub.h")
    set(OUTPUT_LIB "${CMAKE_CURRENT_BINARY_DIR}/libmhs.so")

    add_custom_command(
        OUTPUT ${OUTPUT_LIB} ${OUTPUT_HEADER}
        COMMAND ${CMAKE_COMMAND} -E env
                "MHSDIR=${MICROHS_SRC_DIR}"
                ${MICROHS_BIN}
                -c -optc -shared -optc -fPIC
                -i${MICROHS_SRC_DIR}/mhs
                -i${MICROHS_SRC_DIR}/src
                -i${MICROHS_SRC_DIR}/lib
                -i${MICROHS_SRC_DIR}/paths
                -i${CMAKE_CURRENT_SOURCE_DIR}/src
                Repl -o libmhs.so
        COMMAND pwd && ls
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        COMMENT "Building libmhs.so and ReplHs_sub.h using mhs"
        VERBATIM
    )

    add_custom_target(mhs_build ALL
        DEPENDS ${OUTPUT_HEADER} ${OUTPUT_LIB}
    )

    install(FILES ${OUTPUT_HEADER} DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})
    install(FILES ${OUTPUT_LIB} DESTINATION ${CMAKE_INSTALL_LIBDIR})
endfunction()

