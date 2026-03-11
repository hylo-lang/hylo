include($ENV{IDF_PATH}/tools/cmake/version.cmake)

set(srcs "src/led_strip_api.c")
set(public_requires)

if(CONFIG_SOC_RMT_SUPPORTED)
    list(APPEND srcs "src/led_strip_rmt_dev.c" "src/led_strip_rmt_encoder.c")
endif()

# the SPI backend driver relies on some feature that was available in IDF 5.1
if("${IDF_VERSION_MAJOR}.${IDF_VERSION_MINOR}" VERSION_GREATER_EQUAL "5.1")
    if(CONFIG_SOC_GPSPI_SUPPORTED)
        list(APPEND srcs "src/led_strip_spi_dev.c")
    endif()
endif()

# Starting from esp-idf v5.3, the RMT and SPI drivers are moved to separate components
if("${IDF_VERSION_MAJOR}.${IDF_VERSION_MINOR}" VERSION_GREATER_EQUAL "5.3")
    list(APPEND public_requires "esp_driver_rmt" "esp_driver_spi")
else()
    list(APPEND public_requires "driver")
endif()

idf_component_register(SRCS ${srcs}
                       INCLUDE_DIRS "include" "interface"
                       REQUIRES ${public_requires})
