#lang scribble/base

@title{The Flowmark Language Manual}

@section{Defining text macros in Flowmark}

In Weave there are two kinds of macros, called normal macro and freeform macro respectively.

@subsection{Defining normal macro}

Normal macro is the same as plain-old macros in T64. In Weave, defining a normal macro is done in two steps:
@itemlist[
  @item{Define a form with @tt{\def};}
  @item{Turn the defined form with @tt{\init.macro}.}
]
The syntax for normal macro in Weave is taken from TRAC T84: gaps are represented by integers surrounded with angle brackets @tt{<>}. For example:

@verbatim|{
\def(STR,{The quick brown <2> jumps over the lazy <1>.});
\init.macro(STR);
}|

...is equivalent to this in T64:

@verbatim|{
#(ds,STR,(The quick brown FOX jumps over the lazy DOG.))'
#(ss,STR,DOG,FOX)'
}|

One can also use named gaps like this:

@verbatim|{
\def(STR,{The quick brown <FOX> jumps over the lazy <DOG>.});
\init.macro(STR,DOG,FOX);
}|

The end result is the same.

@subsection{Pieces}

A piece (in Weave terminology) is a minimal semantically meaningful substring. A piece can be one of the followings:

@itemlist[
  @item{A single character that is not a part of any special construct (either by themselves being not a part of any special construct or by escaping with at-sign );}
  @item{A function call, both active and neutral;}
  @item{A whitespace escape sequence;}
  @item{A freeform macro name (explained later);}
]

The concept of piece in Weave is quite important; we'll see this very soon.

@subsection{Forward-reading}

Weave supports forward-reading, which allows text macros themselves to read the upcoming source text themselves instead of delegating the reading to the processing algorithm; this is similar to reader macro in LISPs, the difference being forward-reading occurs at runtime.

@subsection{Freeform macro}

A freeform macro is a kind of "special text macro" that's directly expanded during the execution of the processing algorithm instead of full/partial calling (i.e. by primitives like call and recite.*)

The name for a freeform macro can only contain the following characters:

@itemlist[
    @item{A hash @tt{#}}
    @item{A tilde @tt{~};}
    @item{A backtick @tt{`};}
    @item{A dollar sign @tt{$};}
    @item{A percent sign @tt{%};}
    @item{A circumflex @tt{^};}
    @item{An ampersand @tt{&};}
    @item{An underscore @tt{_};}
]

Although freeform macros do not have the ability to take an argument list, it can still handle the upcoming text by expanding into forward-reading primitives. Consider this example for defining syntax sugar for superscripts, subscripts and math mode in a possible typesetting library; one would define the freeform macro ^, _ and $$ as follows:

@verbatim|{
  \def.free($$,{\toggle(mode.math)});
  \def.free(^,{\format.superscript(\next.piece)});
  \def.free(_,{\format.subscript(\next.piece)});
  $$a^2+b^2=c^2$$
  $$e^{\pi i}=1$$
  $$A_i + B_{ij} <= C_k$$
}|

This would be the equivalent to:
@verbatim|{
  \toggle(mode.math)a\format.superscript(2)+b\format.superscript(2)=c\format.superscript(2)\toggle(mode.math)
  \toggle(mode.math)e\format.superscript(\pi i)=1\toggle(mode.math)
  \toggle(mode.math)A\format.subscript(i) + B\format.subscript(ij) <= C\format.subscript(k)\toggle(mode.math)
}|

@section{Document generation in Flowmark}

Since Weave was originally intended to be the foundation language of a typesetting toolkit like TeX, instead of Standard Output/Standard Error (which are separate but both are normally redirected to the console) like in POSIX-compliant systems, in Weave there's Default Print/Default Neutral/Default Out:

    Default Neutral simply means the neutral string buffer after the execution of the last command group. It's intended for text macro expansion, e.g. having a Weave source file expand into an HTML or Postscript file.
    Default Print always mean the console. This is where you write your output to if you're writing an interactive program.
    Default Out refers the current out port used by the \out primitive ("port" here is a term to refer to a system internal buffer you write to). out ports are intended for generating files (instead of expanding like in Default Neutral).

The content of Default Neutral and Default Out is lost if no target is specified (e.g. by using -e and -o command line options). Ten output port (ID 0~9) is created upon the startup of Weave so one does not need to create new ones most of the time.
Non-text literals in Weave

Weave is largely text-oriented, but there are times when numeric or boolean calculations are needed.

There are four kinds (well technically three) of non-text literals in Weave:

    Integers
    Floating-point numbers (i.e. the ones with decimals)
    Bit vector
    Boolean
        In Weave booleans are actually bit vector of length 1.

All four of them are (kind of) numeric. Primitives that requires numeric arguments would remove the surrounding whitespaces of the texts passed as arguments and tries to interpret it as corresponding numeric values using certain
Valid numeric & boolean literals

    Bit vectors requires the argument text to contain only 0 and 1.
    Boolean requires the argument text, after removing surrounding whitespaces, must be 0 or 1.

Coercion

    Integers,

Keywords

You can use \def.keyword(NAME,ARG1,...,BODY) to define new keywords. The semantics is roughly the same as defining a macro using \def.macro except:

    Keywords are stored in a separate namespace than macros; you can have a keyword and a macro with the same name.
    You can't recite a keyword.

Keywords are intended to have a different syntax for calling; for example, a macro for calculating factorial would be invoked like this:

\call(Factorial,5);

But if Factorial is defined as a keyword, one must invoke it like this:

\Factorial(5);

Keywords are intended to be a mechanism to extend the language itself
Path & import

Weave supports importing other source files (in the form of primitive \import); all the result from the imported file would be carried over. Weave reads the base directory from three source for module name resolving:

    Environment variable WEAVE_IMPORT_PATH;
    The parent directory of the input file;
    Any path added with the \path primitive.

One can specify multiple path in the environment variable WEAVE_IMPORT_PATH by separating them with semicolon ;.

@section{Primitives}

(names tagged with * is still work in progress)

@subsection{Miscellaneous primitives}

@itemlist[
    @item{\halt: Halt any further execution.}
    @item{\debug.list_names*:}
    @item{\set.meta(STR): Returns empty string. Set the meta character to the first character of string STR.}
    @item{\reset.meta: Returns empty string. Set the meta character to semicolon ;, the default used by Weave.}
    @item{\import(MODULE): Returns empty string. Import the target module MODULE. Although it returns empty string, but the effects the imported module has on neutral or any out port will remain.}
    @item{\path(PATH): Returns empty string. Adding PATH to the resolving base list.}
]

@subsection{Form bookkeeping & macro-related primitives}
@itemlist[
    @item{\def(NAME,BODY): Returns empty string. Stores BODY under the name NAME. When NAME is an empty string, this has no effect.}
    @item{\def.free(PAT,BODY): Returns empty string. Used to define freeform macros (explained above). PAT must not be empty, or else an error is reported.}
    @item{\def.macro(NAME,ARG1,...,BODY): Returns empty string. A combination of \def and \init.macro. Equivalent to \def(NAME,BODY)\init.macro(NAME,ARG1,...).}
    @item{\def.keyword(NAME,ARG1,...,BODY): Returns empty string. Used to define a keyword.}
    @item{\init.macro(NAME,ARG1,...): Returns empty string. Used to turn already defined forms into normal macros.}
    @item{\copy(NAME1,NAME2): Returns empty string. Copies the form originally defined under the name NAME1 to the new name NAME2. The newly-defind form has its own form pointer. If NAME1 is not previously defined, an error is reported. If any of the two names are empty string, this has no effect.}
    @item{\move(NAME1,NAME2): Returns empty string. Moves the form from the name NAME1 to the new name NAME2. If NAME1 is not previously defined, an error is reported. If any of the two names are empty string, this has no effect.}
    @item{\del(NAME): Returns empty string. Remove the form defined under the name NAME. If NAME is not previously defined, an error is reported. If =NAME= is an empty string, this has no effect.}
    @item{\del.free(PAT): Returns empty string. Remove a freeform macro with the pattern PAT. If PAT is empty or not previously defined, an error is reported.}
    @item{\del.keyword(NAME) *: Returns empty string. Delete a custom-defined keyword.}
    @item{\del.all: Returns empty string. Remove all definitions, including freeform macros, normal macros and keywords.}
    @item{\del.all_macros: Returns empty string. Remove all normal macros.}
    @item{\del.all_keywords: Returns empty string. Remove all custom-defined keywords.}
    @item{\del.all_free: Returns empty string. Remove all freeform macros.}
]

@subsection{Full calling & partial calling}
@itemlist[
    @item{\call(NAME,ARG1,...): Returns the result of filling in the form defined under the name NAME with its parameters replaced by ARG1, etc.. The full calling primitive; equivalent to cl in T64.}
]

Flowmark has the following partial calling primitives; all of them returns empty string when the form the call is referring to (i.e. the NAME parameter) does not exist:

    \recite.reset(NAME)*: Returns empty string. Resets the form pointer of the form defined under the name NAME. Equivalent to cr in T64.
    \recite.char(NAME,Z)*: Returns the single character pointed by the form pointer of the form defined under the name NAME. The form pointer of NAME is increased by one character. If the form pointer is already at the right-most position, Z is returned instead. Equivalent to cc in T64.
    \recite.nchar(NAME,N,Z) * : Returns the first N character from the form defined under the name NAME starting from the form pointer. N is treated as an integer. If N is not a valid-form integer, a warning is reported, and its first valid-form integer substring is used as a replacement. If there's less than N characters left, they're all returned, and the result would be shorter than N characters. If the form pointer is already at the right-most position, Z is returned instead. Equivalent to cn in T64.
    \recite.next_piece(NAME,Z) * : Returns the next piece after the form pointer of the form defined under the name =NAME=.
    \recite.to_gap(NAME,Z) * :
    \recite.to_pattern(NAME,PAT,Z) * :

@subsection{Forward-reading primitives}

@itemlist[
    @item{\next.piece * :}
    @item{\next.char: Read the next character from the current source file. Returns an empty string when end-of-file is reached.}
    @item{\next.line: Reads till the next linefeed from current source file. The result contains the read linefeed character. Returns an empty string when end-of-file is reached.}
]

@subsection{Algorithmic primitives}

@itemlist[
    @item{\add.int(ARG1,...), \sub.int(ARG1,...), \mult.int(ARG1,...), \div.int(ARG1,...)}
    @item{\add.float(ARG1,...), \sub.float(ARG1,...), \mult.float(ARG1,...), \div.float(ARG1,...)}
    @item{\eq.int(ARG1,ARG2) * , \le.int(ARG1,ARG2) * , \ge.int(ARG1,ARG2) * , \lt.int(ARG1,ARG2) * , \gt.int(ARG1,ARG2) * :}
    @item{\eq.float(ARG1,ARG2) * , \le.float(ARG1,ARG2) * , \ge.float(ARG1,ARG2) * , \lt.float(ARG1,ARG2) * , \gt.float(ARG1,ARG2) * :}
    @item{\and.bit(ARG1,ARG2) * , \or.bit(ARG1,ARG2) * , \not.bit(ARG1) * , \xor.bit(ARG1,ARG2) * :}
    @item{\and(ARG1,...) * :}
    @item{\or(ARG1,...) * :}
    @item{\not(ARG1,...) * :}
    @item{\is.empty(ARG1) * :}
    @item{\is.int(ARG1) * : Returns a boolean value indicating if ARG is a valid integer, 1 means it is valid, 0 means it is not.}
    @item{\is.float(ARG1) * :}
    @item{\is.bit(ARG1) * :}
    @item{\to.bit(ARG1,WIDTH) * : Returns the conversion result of []. WIDTH can be empty; when WIDTH is empty,}
]

@subsection{I/O primitives}

@itemlist[
    @item{\read.str:}
    @item{\read.piece * :}
    @item{\print(X): Returns empty string. Put X to the current print port.}
    @item{\print.form(NAME):}
    @item{\print.free(PAT):}
    @item{\out(X1,X2,...): Returns empty string. Output X1, X2, ... to the current default output port.}
    @item{\set.out(ID): Returns empty string. Set current output port.}
    @item{\reset.out: Returns empty string. Resets the current output port to port 0. (Equivalent to \set.out(0))}
    @item{\new.out: Returns a valid integer string. Creates a new output port. The returned integer string would be the ID of the new output port.}
    @item{\error(X): Returns empty string. Put X to the current error port.}
    @item{\warn(X): Returns empty string. Put X to the current warning port.}
]

@subsection{Branching}

@itemlist[
    @item{\ifeq(STR1,STR2,CLAUSE1,CLAUSE2):}
    @item{\ifeq.int(NUM1,NUM2,CLAUSE1,CLAUSE2):}
    @item{\ifeq.float(NUM1,NUM2,CLAUSE1,CLAUSE2):}
    @item{\ifne(STR1,STR2,CLAUSE1,CLAUSE2):}
    @item{\ifne.int(NUM1,NUM2,CLAUSE1,CLAUSE2):}
    @item{\ifne.float(NUM1,NUM2,CLAUSE1,CLAUSE2):}
]

