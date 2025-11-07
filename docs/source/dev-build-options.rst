..  Copyright (c) 2025,    

   Distributed under the terms of the Apache Software License 2.0.  

   The full license is in the file LICENSE, distributed with this software.

Build and configuration
=======================

General Build Options
---------------------

Building the xeus-haskell library
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``xeus-haskell`` build supports the following options:

- ``XEUS_HASKELL_BUILD_SHARED``: Build the ``xeus-haskell`` shared library. **Enabled by default**.
- ``XEUS_HASKELL_BUILD_STATIC``: Build the ``xeus-haskell`` static library. **Enabled by default**.


- ``XEUS_HASKELL_USE_SHARED_XEUS``: Link with a `xeus` shared library (instead of the static library). **Enabled by default**.

Building the kernel
~~~~~~~~~~~~~~~~~~~

The package includes two options for producing a kernel: an executable ``xhaskell`` and a Python extension module, which is used to launch a kernel from Python.

- ``XEUS_HASKELL_BUILD_EXECUTABLE``: Build the ``xhaskell``  executable. **Enabled by default**.


If ``XEUS_HASKELL_USE_SHARED_XEUS_HASKELL`` is disabled, xhaskell  will be linked statically with ``xeus-haskell``.

Building the Tests
~~~~~~~~~~~~~~~~~~

- ``XEUS_HASKELL_BUILD_TESTS ``: enables the tets  **Disabled by default**.

