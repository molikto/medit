# medit

 The structural editor framework for mlang
 
## build & run

standard SBT project `sbt main/compile`, `sbt main/run`, you need `-XstartOnFirstThread` JVM option on macOS.

## architecture
 
 we are using pretty primitive graphical API, as we want full control of the editor behaviour.
 using a GUI toolkit is not ideal for us.
 
* [lwjgl3](https://github.com/LWJGL/lwjgl3) for windowing
* [JavaCPP Presets for Skia](https://github.com/bytedeco/javacpp-presets/tree/master/skia) for 2D graphics
 
what users need? (in order of implementation!)

* drawing basics ─ draw some text and stuff! can you scroll?
* presentation layout ─ present the content in a non-confusing way
    * automate layout ─ layout rules is in syntax, but the editor will need to layout the code in a 2d plane
    * error feedback
    * state display
* edit & navigate (one way to see is use reflect how you edit yourself)
    * tasks
        * insert, append, delete, replace
            * insert `(` and `.` etc causes a rotation, same with infix operators
            * insert numbers and others will determine in a type directed way
        * wrap, unwrap, unfold, ...
            * wrap is tricky because what do you do with it?
        * navigation
        * selection
        * find replace
        * diff merge
        * copy paste
    * auto complete and tooltips
        * is this match exact?
        * is this the only match?
        * sort matches by typechecking
        * ...
    * devices
        * mouse
        * keyboard
    * modes
        * non-modal
        * modal
    * models
        * text
        * tree
* concrete -─ what MPS calls "structure"
    * generating layout
* GUI
   * file tree
   * mini-map
 
 ## other efforts
 
* IntelliJ MPS
    * this is a structural editor framework, not a language framework, MPS's semantics part is not suited for developing serious general purpose languages from scratch, this project aims to provide an alternative to language implementors an alternative to parser and language server.
    * although it looks like a text editor, but the keyboard handling is not like a text editor at all
* Blockly
* Lamdu
    * very good concepts
    * but why the editor has so much animation? it is a distraction
* Hazel
    * this is too strict with semantics



---------


why I dig this hole?


 
