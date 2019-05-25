#!/bin/bash
set -e

rgbasm -o $1.o $1.asm
rgblink -o $1.gb $1.o
rgbfix -v -p 0 $1.gb
# /home/akmalirfan/Games/BGB/bgb.exe $1.gb
retroarch -L /usr/lib/libretro/gambatte_libretro.so $1.gb