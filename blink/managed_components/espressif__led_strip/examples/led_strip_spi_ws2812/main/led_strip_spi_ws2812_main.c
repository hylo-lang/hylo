/*
 * SPDX-FileCopyrightText: 2023-2024 Espressif Systems (Shanghai) CO LTD
 *
 * SPDX-License-Identifier: Unlicense OR CC0-1.0
 */
#include <stdio.h>
#include "freertos/FreeRTOS.h"
#include "freertos/task.h"
#include "led_strip.h"
#include "esp_log.h"
#include "esp_err.h"

// GPIO assignment
#define LED_STRIP_GPIO_PIN  2
// Numbers of the LED in the strip
#define LED_STRIP_LED_COUNT 24

static const char *TAG = "example";

led_strip_handle_t configure_led(void)
{
    // LED strip general initialization, according to your led board design
    led_strip_config_t strip_config = {
        .strip_gpio_num = LED_STRIP_GPIO_PIN, // The GPIO that connected to the LED strip's data line
        .max_leds = LED_STRIP_LED_COUNT,      // The number of LEDs in the strip,
        .led_model = LED_MODEL_WS2812,        // LED strip model
        // set the color order of the strip: GRB
        .color_component_format = {
            .format = {
                .r_pos = 1, // red is the second byte in the color data
                .g_pos = 0, // green is the first byte in the color data
                .b_pos = 2, // blue is the third byte in the color data
                .num_components = 3, // total 3 color components
            },
        },
        .flags = {
            .invert_out = false, // don't invert the output signal
        }
    };

    // LED strip backend configuration: SPI
    led_strip_spi_config_t spi_config = {
        .clk_src = SPI_CLK_SRC_DEFAULT, // different clock source can lead to different power consumption
        .spi_bus = SPI2_HOST,           // SPI bus ID
        .flags = {
            .with_dma = true, // Using DMA can improve performance and help drive more LEDs
        }
    };

    // LED Strip object handle
    led_strip_handle_t led_strip;
    ESP_ERROR_CHECK(led_strip_new_spi_device(&strip_config, &spi_config, &led_strip));
    ESP_LOGI(TAG, "Created LED strip object with SPI backend");
    return led_strip;
}

void app_main(void)
{
    led_strip_handle_t led_strip = configure_led();
    bool led_on_off = false;

    ESP_LOGI(TAG, "Start blinking LED strip");
    while (1) {
        if (led_on_off) {
            /* Set the LED pixel using RGB from 0 (0%) to 255 (100%) for each color */
            for (int i = 0; i < LED_STRIP_LED_COUNT; i++) {
                ESP_ERROR_CHECK(led_strip_set_pixel(led_strip, i, 5, 5, 5));
            }
            /* Refresh the strip to send data */
            ESP_ERROR_CHECK(led_strip_refresh(led_strip));
            ESP_LOGI(TAG, "LED ON!");
        } else {
            /* Set all LED off to clear all pixels */
            ESP_ERROR_CHECK(led_strip_clear(led_strip));
            ESP_LOGI(TAG, "LED OFF!");
        }

        led_on_off = !led_on_off;
        vTaskDelay(pdMS_TO_TICKS(500));
    }
}
