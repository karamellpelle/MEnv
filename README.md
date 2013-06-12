These are the functions used for the project <https://github.com/karamellpelle/grid>. This code is licensed under the "Simplified BSD License", so that it could be used more freely. But if you do any improvements of the code, I would be happy if these are also released to the public. By placing this code on GitHub, contributing should be easy. Also note that I have not been able to run the code and verify that it works as expected, only that the Haskell code compiles.


Folders
----------------

    GLFW/               : GLFW build
    data/               : data files
    dist/               : (empty)
    iOS/                : iOS build
    source/             : Haskell source


Demo
----------------

The program shows the font rendering, as white text on black background. By touching the screen, the placement of the text is changed. The code should also show how you can write programs (i.e. games) by using IterationStack's.


Building on iOS
----------------

There are two different build scripts: `ghc_build_debug` and `ghc_build_release`. They call the script `ghc_build_` with different parameters. They also accept command line parameters, for GHC. The script `ghc_build_` calls GHC. It relies on ghc-ios (<https://github.com/ghc-ios/ghc/wiki>), and needs to be edited for installation specific paths.

In the folder `iOS/MEnv`, build the Haskell code into an archive `iOS/MEnv/Main.a`:  

    $ cd iOS/MEnv
    $ ./ghc_build_debug

Open the Xcode project in the folder `iOS`, build and run on a connected iOS device. 

The code depend on the following packages:

* parsec        (I have 3.1.3)
* bytestring    (I have 0.10.0.2)
* mtl           (I have 2.1.2)

Hopefully, the program will work on your computer too.


Contact
----------------

<karamellpelle@hotmail.com>
