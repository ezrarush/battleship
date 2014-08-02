#Description

A modern OpenGL networked real time battleship game meant to be a working example of [userial's game protocol](https://github.com/nklein/userial#protocol).  

Changes made to the protocol:

- Ships will be placed on a field of floats and not integers because OpenGL uses floats.
- :match-begin added to :server-opcode to signal the end of ship placement and the beginning of game play 
- Sunk message is sent to opponent regardless of hit so that enemy fire can be displayed.
- Typographical errors corrected to keep naming consistent throughout the code (e.g. :client-opcodes and :server-opcodes changed to :client-opcode and :server-opcode respectively)

These changes may be reverted in a future update.

#How to Play

- Left click on your field to place vertical ships 
- Right click on your field to place horizontal ships
- Click a ship to remove it
- Left click on the enemy's field to fire missiles
- Right click on the enemy's field to ping (hold down right click and drag to determine ping radius)

#How to Run

##SBCL on Windows

Install SBCL and Quicklisp in C:\home\ (https://www.youtube.com/watch?v=VnWVu8VVDbI)

Download this repository and place it in your quicklisp\local-projects\ folder so that quicklisp can find it.  

###Server

Run the following in the command line from the project folder:

```
sbcl --load battleship-server.lisp
```

You may use slime with this SBCL image by uncommenting the two swank lines inside the battleship-server.lisp file and executing the command "slime-connect" in emacs.

### Player One 

Run the following in a second command line instance from the project folder:

```lisp
sbcl --load battleship-client-one.lisp
```

If player one is on a different host than the server, modify battleship-client-one.lisp to use the server's ip address instead of the loop back address "127.0.0.1".

### Player Two

Run the following in a third command line instance from the project folder:

```lisp
sbcl --load battleship-client-two.lisp
```

If player two is on a different host than the server, modify battleship-client-two.lisp to use the server's ip address instead of the loop back address "127.0.0.1".

##Run Scripts Explained

The reason battleship-server.lisp, battleship-client-one.lisp, and battleship-client-two.lisp has (sdl2:make-this-thread-main (lambda () (battleship:main))) is because SBCL does not have a safe way to interrupt a thread and cl-sdl2 must run on the main thread. Here is the work around found at https://github.com/lispgames/cl-sdl2/issues/23