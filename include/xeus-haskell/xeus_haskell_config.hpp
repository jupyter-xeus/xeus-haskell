/***************************************************************************
* Copyright (c) 2025, Masaya Taniguchi                                  
*                                                                          
* Distributed under the terms of the Apache Software License 2.0.                 
*                                                                          
* The full license is in the file LICENSE, distributed with this software. 
****************************************************************************/

#ifndef XEUS_HASKELL_CONFIG_HPP
#define XEUS_HASKELL_CONFIG_HPP

// Project version
#define XEUS_HASKELL_VERSION_MAJOR 0
#define XEUS_HASKELL_VERSION_MINOR 1
#define XEUS_HASKELL_VERSION_PATCH 0

// Composing the version string from major, minor and patch
#define XEUS_HASKELL_CONCATENATE(A, B) XEUS_HASKELL_CONCATENATE_IMPL(A, B)
#define XEUS_HASKELL_CONCATENATE_IMPL(A, B) A##B
#define XEUS_HASKELL_STRINGIFY(a) XEUS_HASKELL_STRINGIFY_IMPL(a)
#define XEUS_HASKELL_STRINGIFY_IMPL(a) #a

#define XEUS_HASKELL_VERSION XEUS_HASKELL_STRINGIFY(XEUS_HASKELL_CONCATENATE(XEUS_HASKELL_VERSION_MAJOR,   \
                 XEUS_HASKELL_CONCATENATE(.,XEUS_HASKELL_CONCATENATE(XEUS_HASKELL_VERSION_MINOR,   \
                                  XEUS_HASKELL_CONCATENATE(.,XEUS_HASKELL_VERSION_PATCH)))))

#ifdef _WIN32
    #ifdef XEUS_HASKELL_EXPORTS
        #define XEUS_HASKELL_API __declspec(dllexport)
    #else
        #define XEUS_HASKELL_API __declspec(dllimport)
    #endif
#else
    #define XEUS_HASKELL_API
#endif

#endif