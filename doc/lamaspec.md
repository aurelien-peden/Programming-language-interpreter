# Spécification du langage LAMA
- C-like
- compilation vers asm x86
- interprétation
- typage statique

# Grammaire

file -> statement\*
statement -> decl | expr | assign-expr

expr -> assign-expr | fun-call | identifier | language-construct
fun-call -> identifier(first-arg? arg\*)
first-arg -> expr
arg -> , expr
assign-expr -> identifier = expr
language-construct -> N/A

