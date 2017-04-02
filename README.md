# Clue-CS3110-Final-Project

List of authors:
- Drew Dunne
- Graham Thomas Rutledge
- Annie Yuan
- Tongtong Lian 

We have included with our project a Makefile in order to make compiling and running our game very simple. The packages you will need installed are listed as below: 

- oUnit
- Yojson
- Str
- Graphics

I believe Str and Graphics come preinstalled, but this is not definite. Yojson and oUnit can be installed with opam. Graphics should come installed. 

On Macs, X11 must be installed. That can be downloaded here: https://www.xquartz.org

To run our program, you can call make with a number of different arguments: 

```
make : runs our test unit suite.
make test : runs our test unit suite. 
make play : runs the game file named game.json in the directory test. 
make play-gui : runs the game file named game.json in the directory test with a graphical user interface rather than the CLI. 
make choose : runs the game, allowing you to later type in the file you wish to load. 
make choose-gui : runs the game, allowing you to later type in the file you wish to load with a GUI. 

make clean : cleans the build. 
```
