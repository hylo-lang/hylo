#include <stdbool.h>
#include <stdint.h>

#if defined(__clang__)
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Weverything"
#endif

#include "freertos/FreeRTOS.h"
#include "freertos/task.h"
#include "driver/gpio.h"

#if defined(__clang__)
#pragma clang diagnostic pop
#endif

void hylo_app_main(void) __attribute__((noreturn));

/// Implementation of Hylo freestanding halt requirement.
void halt(void* r) __attribute__((noreturn)) {
  (void)r;
  while (1) {}
}

/// Converts milliseconds to FreeRTOS ticks.
unsigned int millisToTicks(unsigned int ms) {
  return pdMS_TO_TICKS(ms);
}


void app_main(void)
{
  hylo_app_main();
}

