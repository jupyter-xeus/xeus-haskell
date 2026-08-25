Module.preRun = Module.preRun || [];

Module.preRun.push(function() {
  ENV.MHSDIR = "/share/microhs";
  ENV.MHS_LIBRARY_PATH = "/usr/lib/haskell-packages/microhs";
  Module.ENV.MHSDIR = ENV.MHSDIR;
  Module.ENV.MHS_LIBRARY_PATH = ENV.MHS_LIBRARY_PATH;
});

