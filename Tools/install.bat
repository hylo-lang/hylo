# Compile the support library.
cd ../
swift build -c release --target Support

# Install the support library.
cd %USERPROFILE%
md .val
cd .val
md lib

:: Install the support library.
cd %~dp0
cd ../
cd .build
cd x86_64-unknown-windows-msvc
cd release
cd Support.build
cd src
ren io.cc.o val_support.lib
move val_support.lib %USERPROFILE%/.val/lib
pause
