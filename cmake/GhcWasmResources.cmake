include(CMakeParseArguments)

function(xhaskell_add_ghc_resources target_name)
    set(one_value_args SOURCE_DIR RESOURCE_DIR BUILD_DIR)
    set(multi_value_args RESOURCE_FILES)
    cmake_parse_arguments(GHC_RESOURCES "" "${one_value_args}" "${multi_value_args}" ${ARGN})

    foreach(required_arg SOURCE_DIR RESOURCE_DIR BUILD_DIR)
        if(NOT GHC_RESOURCES_${required_arg})
            message(FATAL_ERROR "xhaskell_add_ghc_resources requires ${required_arg}")
        endif()
    endforeach()
    if(NOT GHC_RESOURCES_RESOURCE_FILES)
        message(FATAL_ERROR "xhaskell_add_ghc_resources requires RESOURCE_FILES")
    endif()

    find_program(XHASKELL_GHC_BASH_EXECUTABLE NAMES bash REQUIRED)
    set(build_script "${GHC_RESOURCES_SOURCE_DIR}/scripts/build-resources.sh")
    set(resource_scripts
        "${build_script}"
        "${GHC_RESOURCES_SOURCE_DIR}/scripts/prepare-inputs.sh"
        "${GHC_RESOURCES_SOURCE_DIR}/scripts/build-runtime.sh"
        "${GHC_RESOURCES_SOURCE_DIR}/scripts/assemble-rootfs.sh")
    get_filename_component(project_dir "${GHC_RESOURCES_SOURCE_DIR}/../.." ABSOLUTE)
    set(resource_stamp "${GHC_RESOURCES_RESOURCE_DIR}/.stamp")

    set(resource_paths)
    foreach(resource_file IN LISTS GHC_RESOURCES_RESOURCE_FILES)
        list(APPEND resource_paths "${GHC_RESOURCES_RESOURCE_DIR}/${resource_file}")
    endforeach()

    add_custom_command(
        OUTPUT "${resource_stamp}"
        BYPRODUCTS ${resource_paths}
        COMMAND "${XHASKELL_GHC_BASH_EXECUTABLE}" "${build_script}"
            --output "${GHC_RESOURCES_RESOURCE_DIR}"
            --build-dir "${GHC_RESOURCES_BUILD_DIR}"
        COMMAND "${CMAKE_COMMAND}" -E touch "${resource_stamp}"
        DEPENDS
            "${project_dir}/licenses.toml"
            "${project_dir}/tools/licenses.py"
            "${GHC_RESOURCES_SOURCE_DIR}/haskell/Playground.hs"
            "${GHC_RESOURCES_SOURCE_DIR}/haskell/XHaskell/Display.hs"
            ${resource_scripts}
        COMMENT "Downloading and building GHC JavaScript-Wasm resources"
        VERBATIM)

    add_custom_target(${target_name} DEPENDS "${resource_stamp}")
endfunction()
