/***************************************************************************
 * Copyright (c) 2025, Masaya Taniguchi
 *
 * Distributed under the terms of the Apache Software License 2.0.
 *
 * The full license is in the file LICENSE, distributed with this software.
 ****************************************************************************/

#include <emscripten/bind.h>

#include "xeus/xembind.hpp"
#include "xeus/xserver_emscripten.hpp"

#include "xghc_interpreter.hpp"

namespace
{
    void notify_listener_async(xeus::xserver_emscripten* server, emscripten::val message)
    {
        server->js_notify_listener(message);
    }
}

EMSCRIPTEN_BINDINGS(xhaskell_ghc_module)
{
    xeus::export_core();
    emscripten::function("xhaskellGhcNotifyListener", &notify_listener_async,
                         emscripten::allow_raw_pointers(), emscripten::async());
    using interpreter_type = xeus_haskell::ghc_interpreter;
    xeus::export_kernel<interpreter_type>("xkernel");
}
