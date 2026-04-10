# LED Strip Driver

[![Component Registry](https://components.espressif.com/components/espressif/led_strip/badge.svg)](https://components.espressif.com/components/espressif/led_strip)

This driver is designed for addressable LEDs like [WS2812](http://www.world-semi.com/Certifications/WS2812B.html), where each LED is controlled by a single data line.

## Supported Backend Peripherals

The LED strip driver supports two different backend peripherals to generate the timing signals required by addressable LEDs:

### The [RMT](https://docs.espressif.com/projects/esp-idf/en/latest/esp32/api-reference/peripherals/rmt.html) Peripheral

This is the most economical way to drive the LEDs because it only consumes one RMT channel, leaving other channels free to use. However, the memory usage increases dramatically with the number of LEDs. If the RMT hardware can't be assist by DMA, the driver will going into interrupt very frequently, thus result in a high CPU usage. What's worse, if the RMT interrupt is delayed or not serviced in time (e.g. if Wi-Fi interrupt happens on the same CPU core), the RMT transaction will be corrupted and the LEDs will display incorrect colors. If you want to use RMT to drive a large number of LEDs, you'd better to enable the DMA feature if possible [^1].

### The [SPI](https://docs.espressif.com/projects/esp-idf/en/latest/esp32/api-reference/peripherals/spi_master.html) Peripheral

SPI peripheral can also be used to generate the timing required by the LED strip, in a so-called "Clock-less" mode. However this backend is not as economical as the RMT one, because it will take up the whole **bus**. You **CANNOT** connect other devices to the same SPI bus if it's been used by the led_strip, because the led_strip doesn't have the concept of "Chip Select".

## Documentation

For detailed information about the LED Strip component, including API reference and user guides, please visit:

-   **Programming Guide & API Reference**: [LED Strip Documentation](https://espressif.github.io/idf-extra-components/latest/led_strip/index.html)
