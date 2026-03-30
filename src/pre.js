// Note from @DerThorsten:
// to make this work with emscripten-4x we just disable all WASMFS related
// code. This makes the loading of the kernel somehow very very slow
// but it will get idle eventually.

// const installLegacyWasmFsMountShim = () => {
//   if (!Module?.FS || typeof Module.FS.mount !== "function") {
//     return;
//   }
//   if (Module.FS.__xhLegacyWasmFsMountShimInstalled) {
//     return;
//   }

//   const originalMount = Module.FS.mount.bind(Module.FS);

//   Module.FS.mount = (type, opts, mountpoint) => {
//     if (!type || typeof type.createBackend === "function") {
//       return originalMount(type, opts, mountpoint);
//     }

//     // jupyterlite-xeus can still pass legacy FS mount objects.
//     if (typeof _wasmfs_create_memory_backend !== "function") {
//       console.warn("[xeus-haskell] WasmFS shim unavailable, using original mount");
//       return originalMount(type, opts, mountpoint);
//     }

//     const typeName = type.name || type.constructor?.name || "legacy-fs-type";
//     console.warn(
//       `[xeus-haskell] WasmFS shim: ${typeName} -> memory backend at ${mountpoint}`
//     );
//     return originalMount(
//       { createBackend: () => _wasmfs_create_memory_backend() },
//       opts,
//       mountpoint
//     );
//   };

//   Module.FS.__xhLegacyWasmFsMountShimInstalled = true;
// };

// const previousPreRun = Module.preRun;
// Module.preRun = () => {
//   console.log("[xeus-haskell] preRun: starting preRun sequence");
//   if (Array.isArray(previousPreRun)) {
//     for (let i = 0; i < previousPreRun.length; i++) {
//       if (typeof previousPreRun[i] === "function") {
//         const f = previousPreRun[i];
//         console.log(`[xeus-haskell] preRun: executing previous preRun function ${f} at index ${i}`);
//         f();
//       }
//     }
//   } else if (typeof previousPreRun === "function") {
//     previousPreRun();
//   }
//   console.log("[xeus-haskell] preRun: setting up environment variables");
//   ENV.MHSDIR = "/share/microhs";
//   ENV.MHS_LIBRARY_PATH = "/usr/lib/haskell-packages/microhs";
//   Module.ENV.MHSDIR = ENV.MHSDIR;
//   Module.ENV.MHS_LIBRARY_PATH = ENV.MHS_LIBRARY_PATH;
//   //installLegacyWasmFsMountShim();
//   console.log("[xeus-haskell] preRun: environment variables set");
// };


Module.preRun = Module.preRun || [];

Module.preRun.push(function() {
    console.log("This runs before the program starts");

    ENV.MHSDIR = "/share/microhs/";
    ENV.MHS_LIBRARY_PATH = "/usr/lib/haskell-packages/microhs";
    Module.ENV.MHSDIR = ENV.MHSDIR;
    Module.ENV.MHS_LIBRARY_PATH = ENV.MHS_LIBRARY_PATH;


});


