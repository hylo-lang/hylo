# LED Strip Programming Guide

## Allocate LED Strip Object with RMT Backend

```c
#define BLINK_GPIO 0

/// LED strip common configuration
led_strip_config_t strip_config = {
    .strip_gpio_num = BLINK_GPIO,  // The GPIO that connected to the LED strip's data line
    .max_leds = 1,                 // The number of LEDs in the strip,
    .led_model = LED_MODEL_WS2812, // LED strip model, it determines the bit timing
    .color_component_format = LED_STRIP_COLOR_COMPONENT_FMT_GRB, // The color component format is G-R-B
    .flags = {
        .invert_out = false, // don't invert the output signal
    }
};

/// RMT backend specific configuration
led_strip_rmt_config_t rmt_config = {
    .clk_src = RMT_CLK_SRC_DEFAULT,    // different clock source can lead to different power consumption
    .resolution_hz = 10 * 1000 * 1000, // RMT counter clock frequency: 10MHz
    .mem_block_symbols = 64,           // the memory size of each RMT channel, in words (4 bytes)
    .flags = {
        .with_dma = false, // DMA feature is available on chips like ESP32-S3/P4
    }
};

/// Create the LED strip object
led_strip_handle_t led_strip = NULL;
ESP_ERROR_CHECK(led_strip_new_rmt_device(&strip_config, &rmt_config, &led_strip));
```

---

You can create multiple LED strip objects with different GPIOs and pixel numbers. The backend driver will automatically allocate sufficient RMT channels for you wherever possible. If the RMT channels are not enough, the [led_strip_new_rmt_device](api.md#function-led_strip_new_rmt_device) will return an error.

## Allocate LED Strip Object with SPI Backend

```c
#define BLINK_GPIO 0

/// LED strip common configuration
led_strip_config_t strip_config = {
    .strip_gpio_num = BLINK_GPIO,  // The GPIO that connected to the LED strip's data line
    .max_leds = 1,                 // The number of LEDs in the strip,
    .led_model = LED_MODEL_WS2812, // LED strip model, it determines the bit timing
    .color_component_format = LED_STRIP_COLOR_COMPONENT_FMT_GRB, // The color component format is G-R-B
    .flags = {
        .invert_out = false, // don't invert the output signal
    }
};

/// SPI backend specific configuration
led_strip_spi_config_t spi_config = {
    .clk_src = SPI_CLK_SRC_DEFAULT, // different clock source can lead to different power consumption
    .spi_bus = SPI2_HOST,           // SPI bus ID
    .flags = {
        .with_dma = true, // Using DMA can improve performance and help drive more LEDs
    }
};

/// Create the LED strip object
led_strip_handle_t led_strip = NULL;
ESP_ERROR_CHECK(led_strip_new_spi_device(&strip_config, &spi_config, &led_strip));
```

---

The number of LED strip objects can be created depends on how many free SPI controllers are free to use in your project.

## FAQ

-   How to set the brightness of the LED strip?
    -   You can tune the brightness by scaling the value of each R-G-B element with a **same** factor. But pay attention to the overflow of the value.
