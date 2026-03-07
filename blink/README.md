# 1) One-time: install Python env + required ESP32-C3 tools + esp-clang

Install Espressif, then:

```
# switch to beta:
cd C:\Espressif\frameworks
git clone https://github.com/espressif/esp-idf.git esp-idf-v6.0-beta2
cd esp-idf-v6.0-beta2
git checkout v6.0-beta2

python C:\Espressif\frameworks\esp-idf-v6.0-beta2\tools\idf_tools.py --idf-path C:\Espressif\frameworks\esp-idf-v6.0-beta2 install-python-env

python C:\Espressif\frameworks\esp-idf-v6.0-beta2\tools\idf_tools.py --idf-path C:\Espressif\frameworks\esp-idf-v6.0-beta2 install --targets esp32c3 required

python C:\Espressif\frameworks\esp-idf-v6.0-beta2\tools\idf_tools.py --idf-path C:\Espressif\frameworks\esp-idf-v6.0-beta2 install --targets esp32c3 esp-clang
```

# 2) Enter ESP-IDF environment

```
& C:\Espressif\frameworks\esp-idf-v6.0-beta2\export.ps1
```

# 3) Build blink with clang toolchain

```
idf.py -C C:\Espressif\frameworks\esp-idf-v6.0-beta2\examples\get-started\blink -DIDF_TOOLCHAIN=clang set-target esp32c3

idf.py -C ./ -DIDF_TOOLCHAIN=clang set-target esp32c3

idf.py -C C:\Espressif\frameworks\esp-idf-v6.0-beta2\examples\get-started\blink -DIDF_TOOLCHAIN=clang build

idf.py -C ./ -DIDF_TOOLCHAIN=clang build
```

# 4) Flash (COM3 from your machine)

```
idf.py -C C:\Espressif\frameworks\esp-idf-v6.0-beta2\examples\get-started\blink -DIDF_TOOLCHAIN=clang -p COM3 flash
```

# 5) Monitor logs

```
idf.py -C C:\Espressif\frameworks\esp-idf-v6.0-beta2\examples\get-started\blink -p COM3 monitor
```
