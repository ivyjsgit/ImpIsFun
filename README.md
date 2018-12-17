##ImpIsFun
ImpIsFun is an extension of the Imp language that was created for Project 11 for PL.


###Changes from Imp
___
* ":=" is replaced with "="
* The not operator is now "~". It was made this way, since this language was originally going to be a interpretted version of the Jack programming language from CSO. I found that rewriting an entire language is tough, and focused on extending the IMP language through the use of subroutines.

###Writing Functions
___
To create a function, you use the syntax

```
function type functionName (type arg1*, type arg2*){
...
return var;
}
```

* You ___MUST___ have a return type. There are no void functions. In this case, you just ignore the output of a function.
* ___"*"___ represents an optional argument to the function. You do not have to have these.
* Functions can only use variables in their scope. Ex: you can't borrow a variable from another function.

###Calling Functions
___
To call a function, you use the syntax
```
type s;
s = call function(*,*);
```

* "*" represents an optional argument to the function. The number you insert __MUST__ match the number and type in the function definition.
* Functions can not be an argument of a function.
* These are expressions, so you can do things like
```
s = call function(1,1+2);
```

###Challenges
___
* This project was quite hard. In order to finish this project, I had to greatly understand the work that we did in project 11.
*  Initially, I wanted the functions to act like macros, but it ended up being much much easier for functions to act like functions.
*  Another difficult part of the project was writing the parsers for function definitions, since I hadn't had to parse a list of items before.
*  I had to add a lot of code that was unneed for 90% of code, but was needed for some cases relating to returning, so this proved tedious.
*  A lot of the work of the project takes part in the typechecking part of the program. For example, handling arguments and function types is handled here.
*  It was hard to bend my mind around this project, as I didn't quite understand how the typechecker worked the first time around.

###Usage
___
* A program written in this language can have any extension
* The basic syntax of the language is based upon Imp, so you can write programs in Imp syntax with the noted changes and the addition of functions.
* Use the following code at the GHCI prompt to run a program:

```haskell
run "filename.imp"
```
###Scrapped stuff that I spent my time on
___
* I spent a bit of time trying to add other data types such as Strings and Arrays to the program. However, I found some issues since interpExpr is of type


```haskell 
interpExpr :: Mem -> Expr -> World -> Value
```
* Because of this, along with difficulty in changing the code that runs interpExpr, I decided not to do this.
* This proved to be right, as adding functions to the language took up ___way___ more of my time than I expected.
* Functions changed from Macros to Subroutines
* The language changed from being based on Jack from CSO's NandToTetris to a language based on Imp. This proved to be much more worthwhile to persue, as I couldn't have all of the datatypes as the Jack language, nor did I have time to implement classes

###Author
___
This program was made by Ivy J in Dr. Yorgey's PL class