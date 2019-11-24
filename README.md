# medit

 The structural editor framework for mlang
 
## build & run

standard SBT project `sbt main/compile`, `sbt main/run`, you need `-XstartOnFirstThread` JVM option on macOS.

## architecture
 
 we are using pretty primitive graphical API, as we want full controll of the editor behaviour.
 using a GUI toolkit is not ideal for us.
 
 * [lwjgl3](https://github.com/LWJGL/lwjgl3) for windowing
 * [JavaCPP Presets for Skia](https://github.com/bytedeco/javacpp-presets/tree/master/skia) for 2D graphics
 
 ## FAQ
 
* differences with IntelliJ MPS?
    * this is a structural editor framework, not a language framework, MPS's semantics part is 
    not suited for developing serious general purpose languages from scratch, this project aims
    to provide an alternative to language implementors an alternative to parser and language server.


 
