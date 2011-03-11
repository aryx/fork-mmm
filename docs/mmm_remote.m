.TH MMM_REMOTE 1

.SH NAME
mmm_remote \- Remote control for the MMM browser

.SH SYNOPSIS
.B mmm_remote
[
.B \-get
|
.B \-getbody
|
.B \-head
|
.B \-show
]
.B URL

.SH DESCRIPTION
.B mmm_remote
is a companion program for the
.B MMM browser.
It provides simple remote-control facilities, assuming
.B mmm
has been run with
.B \-external
option.

The requested action (or
.B \-show
by default) is performed on the given URL.
.B \-get
returns the full HTTP response, including the headers, as if
.B mmm
was a proxy.
.B -getbody
returns only the body of the document.
.B -head
performs a HEAD request, and thus returns only the headers.
.B -show
instructs
.B MMM
to open a new window on that URL.

.SH SEE ALSO
.BR mmm (1),
.BR htparse (1)
