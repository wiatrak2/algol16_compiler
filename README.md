Algol 16 cross compiler
=======================


This is cross compiler of Algol 16 programming language. The compiler is written in Prolog and is destied to Sextium® III processor designed by Mr Tomasz Wierzbicki.

----------


About Algol 16
-------------

The only type in Algol 16 is integer. There are procedures in Algol 16 (also nested ones), which return integer value.  If we don't mind the result of the procedure we use key word **call**. Currently there are only global variables, which can be declarated with hey word **local**.

If there is an expresion **return e** in procedure body, the program calculates the value of *e*, ends the procedure call and reutrns value of *e* wherever the procedure was called.

How to write an Algol 16 program ?
----------------------------------

Well, every program written in Algol 16 has to begin with key word **program**. After that we can declarate the global procedures using **procedure** and variables with key word **local**.  After that we need to place the main block of our program. Every block in Algol 16 begins with **being** and last till **end** For instance, the beginnig of simple Algol 16 program could look like this:
``` 
program GCD
local a, b, c
begin
```
OK, now it would be lovely to write the main body of program. The syntax of Algol 16 is similar to C language and fully described few lines below. Let's continue our program:
``` 
program GCD
local a, b, c
begin
	read a;
	read b;
	while b <> 0 do
		c := a mod b;
		a := b;
		b := c
	done;
	write a
end
```
Great! As you can see we give out GCD program two variables and it calculates the greatest common divisor of them using [Euclidean algorithm]. But what about procedures? You can declarate them before the beginnig of the main program block.  A simple declaration of a procedure looks like:
```
program Procedure
local n
	procedure Proc ()
```
Just like the main block of program every procedure contains it's own declaration *(which can be only another procedure in this version of compiler)* and a block. So let's write a simple program with a nasted procedure.
```
program Procedure
local n
	procedure Proc()
		procedure Nested_Proc()
		begin
			write n div 4
		end
	begin
		write n;
		call Nested_Proc();
		return 42
	end
begin
	n := 8;
	write Proc()
end
```
This program uses one variable **n** which equals 8. In the main block we call procedure **Proc** what causes writing 8 and then calling the **Nested_Proc**. In next step we will se 2 (which equals 8 / 4) in our outupt. At the end the procedure Proc returns constant 42 which means that the instruction **write Proc()** will ends with displaying 42.

Both of these programs are in *programs* directory.

Algol 16 BNF
------------

```
Algol 16 language
Tokens:
		- operators: + - * < <= > >= = <> := ;
		- stop symbols: , ( )
		- key words: and begin call div do done else end fi if local mod not or procedure program read return then value while write
    	- comments: char sequences started with (* and ended with *)
   		- identificators: sequences of ASCII signs, digits 0-9, signs _ and '
   		- constant:	nonempty sequences of digits 0 .. 9

<program> 				::= 	program <id> <block>
<block> 				::= 	<declarations> begin <complex instruction> end
<declarations> 			::= 	<empty> | <declarations> <single declaration>
<single declaration> 	::=		<declarator> | <procedure>
<declarator>			::=		local <variables>
<variables>				::=		<single variable> | <variables> , <single variable>
<single variable>		::=		<id>
<procedure>				::= 	procedure <procedure id> ( ) <block>
<procedure id>			::= 	<id>
<complex instruction>	::=		<instruction> | <complex instruction> ; <instruction>
<instruction>			::=		<single variable> := <arithmetic expresion>
						|		if <bool expresion> then <complex instruction> fi
						|		if <bool expresion> then <complex instruction> else <complex instruction> fi
						|		while <bool expresion> do <complex instruction> done
						|		call <procedure call>
						|		return <arithmetic expresion>
						|		read <sigle variable>
						|		write <arithmetic expresion>
<arithmetic expresion>	::=		<summand> | <arithmetic expresion> <additive op> <summand>
<additive op>			::=		+ | -
<summand>				::=		<factor> | <summand> <multiplicative op> <factor>
<multiplicative op>		::=		* | div | mod
<factor>				::=		<simple expresion> | - <simple expresion>
<simple expresion>		::=		<atom expresion> | ( <arithmetic expresion> )
<atom expresion>		::=		<single variable> | <procedure call> | <constant>
<procedure call>		::=		<procedure id> ( )
<bool expresion>		::= 	<conjunction> | <bool expresion> or <conjunction>
<conjunction>			::=		<condition> | <conjunction> and <condition>
<condition>				::=		<relative expresion> | not <relative expresion>
<relative expresion>	::=		<arithmetic expresion> <relative op> <arithmetic expresion> | ( <bool expresion> )
<relative op>			::=		< | <= | > | >= | = | <>
<empty>					::=	
```

Sextium® III
-------

![Logo](http://i.imgur.com/h2kGXNR.png)

Full description of Sextium® III in exercise contest (in Polish)

```
Sextium® III Processor:

ACC -  accumulator
DR  -  data register
AR  -  address register
PC  -  program counter
MEM -  memory
IO  -  input / output

Sextium® III Instructions:

Instruction 	Symbol 		Definition

0 				NOP 		Do nothing
1 				SYSCALL		syscall(ACC)
2				LOAD		MEM[AR] -> ACC
3				STORE		ACC -> MEM[AR]
4				SWAPA		ACC <-> AR
5				SWAPD		ACC <-> DR
6				BRANCHZ		if ACC = 0 then AR -> PC
7				BRANCHN		if ACC < 0 then AR -> PC
8				JUMP		ACC -> PC
0 				CONST 		MEM[PC++] -> ACC
A 				ADD 		ACC + DR -> ACC
B 				SUB 		ACC - DR -> ACC
C 				MUL 		ACC * DR -> ACC
D 				DIV 		ACC / DR -> ACC
E 				SHIFT 		if DR < 0 then ACC >> -DR else ACC << DR
F 				NAND		~(ACC & DR)

Sextium® III system functions:

ACC 	Symbol 		Definition

0 		HALT		Stop
1 		READ 		IO -> ACC
2 		WRITE 		DR -> IO

```

How to use the compiler ?
=======

The compiler is written in Prolog language so you could need [this]. Also Sextium® III files will be needed. To get the sequence of Sextium® III instruction use `algol16/2` predicate. The compiler will return the list of integer. However to run our program we need this list as hexadecimal numbers. Fortunately there is `dec_to_hex/1` predicate in the compiler. So now we can do that:
```
?- algol16(`>>>OUR PROGRAM<<<`, X), dec_to_hex(X).
```
And we will get the sequence of instruction for processor. Now we can make a Sextium® III execution file in very simple way:
```bash
echo "SEQENCE OF INSTRUCTION" | xxd -p -r >example.sextium
```
and run the program with:
```bash
./sextium example.sextium
```
Compiling GCD program:
![enter image description here](http://i.imgur.com/AqXo8gv.png)

![enter image description here](http://i.imgur.com/ivG1kIR.png)

The compiler may be developed in the future.

Wojciech Pratkowiecki
II UWr

[Euclidean algorithm]: https://en.wikipedia.org/wiki/Euclidean_algorithm
[this]: http://www.swi-prolog.org

