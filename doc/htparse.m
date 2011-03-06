.TH HTPARSE 1

.SH NAME
htparse \- A simple HTML syntax checker

.SH SYNOPSIS
.B htparse
[
.B \-strict
]
[
.B \-struct (0|1|2)
]
[
.B \-depth n
]
[
.B \-v
]
.I file ...

.SH DESCRIPTION
.B htparse
is essentially the HTML parser used by the
.B MMM browser.
It shows what it thinks are lexical and syntax errors in the HTML files
given as input.
.B This is not
a SGML validator per se, although the parsing applies SGML minimisation
rules according to a built-in DTD (currently HTML 3.2 with a couple of
extensions for <EMBED> and frames). In particular, existence and/or
values of tag attributes are
.B not checked,
order of elements is 
.B not checked,
(e.g. you can have multiple BODY and HEAD in any order).

.B -strict
makes lexical analysis stricter on characters allowed in attribute names.

.B -struct n
produces a pretty-printed abstracted version of the document, where only
tags remain (including those tags added because of minimisation rules).
.B n
varies the pretty-print layout, and
.B -depth d
changes the ellipsis level.

After loading the
.B html-error.el
file in Emacs, you can "compile" an HTML file with
.B htparse
as command, and then go around looking at errors with traditionnal
.B next-error
Emacs navigation.

.SH BUGS
This is not an HTML validator. Do not under any circumstance claim that
your HTML is correct because it passes
.B htparse.


.SH SEE ALSO
.BR mmm (1)
