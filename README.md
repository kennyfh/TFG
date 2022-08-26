# Parallel Functional Programming on GPUs

-   **Author**: Kenny Jesús Flores Huamán
-   **Mentor**: Miguel Ángel Martínez-del-Amor

# Description

The use of parallel programming over GPU’s is everywhere, from video editing to the creation of IA models to detect frauds in actual time, including graphic generation for video games. 

This project will study, analyse and experiment on the performance, flexibility and design of parallel algorithms over CPU and GPU from the functioning paradigm, since this paradigm offers certain properties that are good for adjusting with parallelism. 

In order to explore the use of parallel functioning programmation, parallel algorithms have been designed for processing digital images with REPA and Accelerate, two written libraries in Haskell. 

# Dependencies
 
 For the correct functioning of this project, the use of operating systems derived from Ubuntu/Debian is recommended, in addition to having an NVIDIA graphics card that is compatible with CUDA technology.
 
## Haskell and Stack Platform

In addition to having the Haskell language installed, we must install Stack, a tool used in creating the Haskell project and managing dependencies within the project. If you don't have it in the system, run the following commands:

```bash
sudo apt-get install haskell-platform
wget -qO- https://get.haskellstack.org/ | sh
```
## LLVM 

To be able to run programs in parallel on CPU/GPU in Accelerate it's necessary to install LLVM, a mature optimizing compiler that is focused on various architectures. So the following external libraries will be required:

- [LLVM](https://llvm.org/)
- [libFFI](https://sourceware.org/libffi/)

## CUDA

It is necessary to have the CUDA tools installed in our system. You can find it on the [official NVIDIA page](https://developer.nvidia.com/cuda-downloads).

## FFMPEG

To read and write videos, it's necessary to have install FFmpeg.

# Usage

Once the above dependencies are installed and the project downloaded, the analyzes can be executed using the following command from the root folder of the project:

```bash
stack run
```

