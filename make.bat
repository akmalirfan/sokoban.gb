C:\rgbds\rgbasm.exe -o %1.o .\%1.asm
C:\rgbds\rgblink.exe -o %1.gb %1.o
C:\rgbds\rgbfix -v -p 0 %1.gb
.\%1.gb