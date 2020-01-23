# medit

[![Actions Status](https://github.com/molikto/medit/workflows/main/badge.svg)](https://github.com/molikto/medit/actions)

The structural editor framework for mlang
 

I'm doing a [devlog](https://molikto.github.io/posts/05-growing-a-structural-editor.html) to record how it evolves.
 
 
 
## build & run

standard SBT project `sbt gui-jvm/compile`, `sbt gui-jvm/run`, you need `-XstartOnFirstThread` JVM option on macOS.


## MVP for incremental-parsing refactor


the goal is to experiment if this is a good way of doing it. it is not indended to create practical solutions for existing languages yet, after we know that this will play out, existing language & real usage is next step

* keep using Skia, look into JS based stack
* just start with add tree-sitter to java, then try to came up with a dsl for:
    * translate to tree-sitter
    * layout combinators
    
next step is to expand it into a language benchmark like 

* retrofit existing text based languages
    * retrofit for language servers
    * retrofit for debug servers
* abstract dependencies
    * gui
    * incremental parser

    
---- 

*old stuff*

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
            * regular expressions
        * diff merge
        * copy paste
    * auto complete and tooltips
        * is this match exact?
        * is this the only match?
        * sort matches by typechecking
        * ...
    * modes
        * non-modal
        * modal
    * models
        * text
        * tree
    * undo redo
* GUI
    * tabs
    * file tree
    * mini-map
   
more

* non-tree layout like table or matrix
 
## code

firstly, the AST structure is defined using combinators `arr`, `opt`, `bag` and named record and sum types. actually, this language to define AST can define the language itself. and this is what we have done. see `Type.scala` for the AST in Scala code, and for meta definition see `langauge-meta.json`. being self-describable means if you want to define a new language, you can use the structural editor to edit the definition. this is like JetBrains MPS, where you define your language itself inside the structural editor.

to bootstrap with minimal effort, we first view the editor as a tree editor, forgetting layout issues. after bootstrapping, we continue to add layout definitions.

`Node` is a tree where each node has a type of structured which is a instance of named structure syntax, or collection, which is for combinator types`arr`, `opt` etc

then from the template, it generates `Frag`, and it is used for layout purpose.

then we add ways to resolve insert point from mouse.

* NEXT
   * handle all insertion points
      * what user types? separators, delimiters, keyword, choice, const
   * combine delimiter and separator, but with more options
   * handle keyboard up, down, right, left
   * handle choice
   * handle reference



---------


why I dig this hole?


 
