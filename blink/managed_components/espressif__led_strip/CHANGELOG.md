## 3.0.3

- Support WS2816 with 16-bit color

## 3.0.1

- Support WS2811 bit timing

## 3.0.0

- Discontinued support for ESP-IDF v4.x
- Added configuration for user-defined color component format

## 2.5.5

- Simplified the led_strip component dependency, the time of full build with ESP-IDF v5.3 can now be shorter.

## 2.5.4

- Inserted extra delay when initialize the SPI LED device, to ensure all LEDs are in the reset state correctly

## 2.5.3

- Extend reset time (280us) to support WS2812B-V5

## 2.5.2

- Added API reference doc (api.md)

## 2.5.0

- Enabled support for IDF4.4 and above
  - with RMT backend only
- Added API `led_strip_set_pixel_hsv`

## 2.4.0

- Support configurable SPI mode to control leds
  - recommend enabling DMA when using SPI mode

## 2.3.0

- Support configurable RMT channel size by setting `mem_block_symbols`

## 2.2.0

- Support for 4 components RGBW leds (SK6812):
  - in led_strip_config_t new fields
      led_pixel_format, controlling byte format (LED_PIXEL_FORMAT_GRB, LED_PIXEL_FORMAT_GRBW)
      led_model, used to configure bit timing (LED_MODEL_WS2812, LED_MODEL_SK6812)
  - new API led_strip_set_pixel_rgbw
  - new interface type set_pixel_rgbw

## 2.1.0

- Support DMA feature, which offloads the CPU by a lot when it comes to drive a bunch of LEDs
- Support various RMT clock sources
- Acquire and release the power management lock before and after each refresh
- New driver flag: `invert_out` which can invert the led control signal by hardware

## 2.0.0

- Reimplemented the driver using the new RMT driver (`driver/rmt_tx.h`)

## 1.0.0

- Initial driver version, based on the legacy RMT driver (`driver/rmt.h`)
