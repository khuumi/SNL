SNL (Stage (null) Language) Project Proposal
===================================
Alex Liu, al3037 (Language Guru)
Andre Paiva, app2125 (Verification & Validation)
Daniel Maxson, dsm2157 (System Architect)
James Lin, jl3782 (Manager)

COMS W4115 - Programming Languages and Translators

This compiler was our final project for Professor Edwards' course at Columbia University in the Fall of 2014. The goal was to design a language and compiler from the ground up using OCaml and compilation into a standard modern programming language. Feel free to take a look and get some inspiration. If you're feeling brave, try out our language and see what you think!

**Introduction**

SNL is a language designed to model role-playing game scenarios based on the structure of state machines. It allows users to programmatically re-create sequential scenarios as “stories.” Its syntax aims to be simple so that adolescents can easily understand how to use it. Where possible, SNL uses intuitive keywords in place of symbols, since symbols commonly known in the CS community can be confusing to those who are unfamiliar with programming. This will encourage children to learn how to write code in a controlled, fun environment while allowing them to exercise their creativity.

**So What Exactly Is This?**
This contains all the source code for our compiler. In it, you will find all the files you need to make our SNL compiler. A good place to start for all of this is in our final report (alternately, take a look at our language reference model if you have already typed "make"). From here, you'll be coding in SNL in no time at all.

**Compiler Setup**
To compile the compiler, utilize the "make" command. This will create the SNL compiler "snlc".

$ make

**Compile SNL to Java**
To compile SNL code to Java code, run the SNL compiler with the "-j" flag on the SNL file and the Java files will appear in the same level directory. Use the flag "--output_path <path>" to designate a specific path for the Java files. This can then be manually compiled with "javac" given that the file SNLObject.java is in the same level as "snlc".

$ ./snlc -j program_name.snl <--output_path> <path>

**Compile SNL to Java Executable**
To compile SNL code to a Java executable, run the SNL compilation script on the SNL file. You can then run this executable using Java.

$ ./snl program_name.snl

To run the program, enter: java program_name

$ java program_name
