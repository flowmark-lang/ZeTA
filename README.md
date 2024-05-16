ZeTA
====

ZeTA is an implementation of Flowmark for typesetting documents.

## Build

Main executable:

```
raco exe ./main.rkt
raco distribute dist ./main
```

Documentation:

```
# HTML
raco scribble --htmls --dest . --dest-name docs ./scribblings/flowmark-zeta.scrbl
# or:
raco scribble --html --dest docs --dest-name index.html ./scribblings/flowmark-zeta.scrbl

# PDF
raco scribble --pdf --dest ./docs --dest-name manual.pdf ./scribblings/flowmark-zeta.scrbl

# Markdown
raco scribble --markdown --dest ./docs --dest-name manual.md ./scribblings/flowmark-zeta.scrbl
```

