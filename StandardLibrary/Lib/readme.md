This folder contains static libraries that can be used by `hc` to link the Hylo object files against.

- [pthreadVCE3.lib](pthreadVCE3.lib): PThread polyfill library for Windows. Also provides LibC functions
  `aligned_alloc`, `free`, `fdopen`. The threading functionality has some issues