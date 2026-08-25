include(FetchContent)

function(fetch_ut)
    set(UT_VERSION "v2.3.1")
    set(UT_URL "https://github.com/boost-ext/ut/archive/refs/tags/${UT_VERSION}.tar.gz")
    FetchContent_Declare(
      ut
      URL ${UT_URL}
      URL_HASH SHA256=e51bf1873705819730c3f9d2d397268d1c26128565478e2e65b7d0abb45ea9b1
      DOWNLOAD_EXTRACT_TIMESTAMP TRUE
    )
    FetchContent_MakeAvailable(ut)
endfunction()
