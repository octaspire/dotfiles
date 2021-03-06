" Vim syntax file for Octaspire Dern programming language

if exists("b:current_syntax")
    finish
endif

setlocal iskeyword=33-39,42-90,92,94-126

syn match dernChar "\v\|.\|"
syn match dernChar "\v\|newline\|"
syn match dernChar "\v\|tab\|"
syn match dernChar "\v\|bar\|"
syn match dernChar "\v\|string\-start\|"
syn match dernChar "\v\|string\-end\|"
syn match dernChar "\v\|[0-9a-fA-F]+\|"
hi link dernChar Character

syn region dernString contains=dernEscape start=/\v\[/ skip=/\v\|\]\|/ end=/\v\]/
syn match dernEscape "\v\|.\|" contained
syn match dernEscape "\v\|newline\|" contained
syn match dernEscape "\v\|tab\|" contained
syn match dernEscape "\v\|bar\|" contained
syn match dernEscape "\v\|string\-start\|" contained
syn match dernEscape "\v\|string\-end\|" contained
syn match dernEscape "\v\|[0-9a-fA-F]+\|" contained
syn match dernEscape "\v\{\}" contained
hi link dernString String

syn keyword dernKeyword != * + ++ += - -- -= -== / < <= = == === > >= abort and acos asin atan cos define distance do doc env-current env-global env-new eval exit find fn for hash-map if len mod not ln@ cp@ or pop-front pow print println quote read-and-eval-path read-and-eval-string return select sin sqrt starts-with? string-format tan to-integer to-string uid vector while io-file-open port-read port-write port-seek port-flush port-close port-dist port-length input-file-open output-file-open port-supports-output? port-supports-input? require queue queue-with-max-length list howto howto-ok howto-no as in pop-back
hi link dernKeyword Keyword

syn keyword dernBoolean true false nil
hi link dernBoolean Boolean

syn match dernReal "\v\d+\.\d+"
hi link dernReal Float

syn match dernInt "\v\d+"
hi link dernInt Number

syn match dernVarArg "\v \.\.\."
hi link dernVarArg Delimiter

syn match dernSexpr "\v\("
syn match dernSexpr "\v\)"
hi link dernSexpr Delimiter

syn match dernComment "\v;.*\n"
hi link dernComment Comment

syn region dernMultilineComment start="\#\!" end="\!\#"
hi link dernMultilineComment Comment

let b:current_syntax = "dern"

