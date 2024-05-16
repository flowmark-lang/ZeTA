#lang scribble/base

@title{Getting Started}

(very early draft. subject to change.)
@section{Introduction}

Flowmark is a macro language influenced by TRAC T64.

@section{A taste of Flowmark}

The following is an example of the factorial function:

@verbatim|{
\def(Factorial,{\
  \cond(\
    \eq.int(<1>,0),0,\
    \eq.int(<1>,1),1,\
    1, {\mult.int(<1>,\call(Factorial,\sub.int(<1>,1)))}\
  )\
});
\init.macro(Factorial);
\print(\call(Factorial,5));
}|

The following is an example of a solution to the Tower of Hanoi problem.

@verbatim|{
\def.free($,{\print({
})});
\def(Hanoi,\
  {\cond(\
    \eq.int(<1>,0),,\
    \eq.int(<1>,1),{\print(Move from <from> to <to>)$},\
    1,{\call(Hanoi,\sub.int(<1>,1),<from>,<via>,<to>)\
       \print(Move from <from> to <to>)$\
       \call(Hanoi,\sub.int(<1>,1),<via>,<to>,<from>)}\
  )}\
);
\init.macro(Hanoi,,from,to,via);
\print(\call(Hanoi,3,A,C,B));
}|

@section{Difference between Weave and TRAC T64}

@itemlist[
  @item{Hashes # that starts a function call is replaced with slashes \\.}
  @item{Function call syntax is slightly different (\func(arg1,arg2,...) vs. #(func,arg1,arg2,...)).}
  @item{Default meta character in Flowmark is semicolon ; instead of apostrophe '.}
  @item{In T64 spaces are preserved, forcing many TRAC source code to be left-aligned. In Flowmark, any consequential whitespaces after a slash \ is ignored altogether; this allows one to indent their code; if whitespaces are needed, one could always use the protective parentheses.}
  @item{The at-sign  is used as some kind of "global escape character"; it is guaranteed that the next character after an at-sign (except when it's in protective parentheses) is retained regardless of any syntax rules and previously defined macros.}
  @item{The way to define text macros is slightly different; in Flowmark it's like a combination of T64 and T84. (explained later)}
  @item{It's possible to extend the processing algorithm to a limited content by using something called a /freeform macro/. (explained later)}
]

@section{Difference between Flowmark and TeX}

@itemlist[
  @item{Whitespaces are always significant (i.e. ); }
]

