:: Install the support library.
cd D:\a\val\val\.build\x86_64-unknown-windows-msvc\release\Support.build\src
md .val
cd .val
md lib

:: Install the support library.
cd ..\
ren io.cc.o val_support.lib
move val_support.lib D:\a\val\val\.build\x86_64-unknown-windows-msvc\release\Support.build\src\.val\lib
pause
