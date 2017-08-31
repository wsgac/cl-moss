CL-MOSS
=======

About
-----
This is a Common Lisp utility for submitting code to Stanford's [MOSS](https://theory.stanford.edu/~aiken/moss/) (Measure Of Software Similarity) software similarity detection system. It allows submitting multiple source code files in order to generate a similarity report. The MOSS system only runs on Stanford's servers - you cannot run your own instance. Hence the need for a submission script. This one is mostly a Common Lisp rewrite of a PHP script written by Philipp Helo Rehs (available [here](https://github.com/Phhere/MOSS-PHP)).

Supported Languages
-------------------
Currently supported languages are: C, C++, Java, ML, Pascal, Ada, Lisp, Scheme, Haskell, Fortran, ASCII, VHDL, Perl, Matlab, Python, MIPS assembly, Prolog, Spice, VB, C#, Modula2, a8086 assembly, JavaScript, PLSQL and Verilog

Installation
------------
CL-MOSS is ASDF-based. The simplest way to install it is via Quicklisp by running:
```
CL-USER> (ql:quickload :cl-moss)
```
If you have a CL-MOSS repo cloned somewhere (e.g. for hacking), you can also make Quicklisp aware of it, e.g. by creating a symlink to it in your `local-projects` directory and then run:
```
CL-USER> (ql:quickload :cl-moss)
```

Example Usage
-------------
Below is a sample usage session:
```
CL-MOSS> (defparameter *moss* (moss-initialize "<user-id>"))
CL-MOSS> (set-language *moss* :lisp) ; string value is also fine
CL-MOSS> (add-base-file *moss* "project/example.lisp")
CL-MOSS> (add-by-wildcard *moss* "project/src/*.lisp")
CL-MOSS> (set-comment-string *moss* "Let's analyze some code similarities!")
CL-MOSS> (send *moss*)
-> "http://moss.stanford.edu/results/<submission-id>"
```
Last step should yield a URL where you can view the results of the analysis of your submitted files.
