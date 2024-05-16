#lang scribble/base

@title{The ZeTA User Manual}

@section{Introduction to ZeTA}

@section{The document model}

@itemlist[
  @item{Each document is comprised of multiple @bold{subdocument}s. Some subdocument continue onto multiple pages (e.g. common header/footer, main content) and some dont.}
  @item{A model for a subdocument contains the following things:
    @itemlist[
      @item{A @bold{width}.}
      @item{Whether this subdocument is @bold{open-bottom} or not. "Open-bottom" in ZeTA jargon means it does not have a fixed height.}
      @item{Margins of the four sides.}
      @item{The font map used within this subdocument.}
    ]
  }
]

@section{The typesetting process}


