"-----------------------------------------------------------------------------
" This license doesn't apply to the bottom part of this file
"-----------------------------------------------------------------------------
"  Octaspire dotfiles - Various configuration files
"  Copyright 2017 www.octaspire.com
"
"  Licensed under the Apache License, Version 2.0 (the "License");
"  you may not use this file except in compliance with the License.
"  You may obtain a copy of the License at
"
"     http://www.apache.org/licenses/LICENSE-2.0
"
"  Unless required by applicable law or agreed to in writing, software
"  distributed under the License is distributed on an "AS IS" BASIS,
"  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
"  See the License for the specific language governing permissions and
"  limitations under the License.
"-----------------------------------------------------------------------------
set cst

set fileencodings=utf-8

let mapleader=" "

if has("autocmd")
    au BufNewFile,BufRead *.dern set filetype=dern
    au filetype dern set lisp
    au filetype dern set autoindent
endif

"Find files from subdirectories
set complete+=k**/*

syntax enable
set t_Co=256
"let g:solarized_termcolors=256
colorscheme default
silent! colorscheme solarized
set background=dark

set tabstop=4
set shiftwidth=4
set expandtab
set smarttab
set showcmd
set number
set showmatch
set hlsearch
set incsearch
set ignorecase
set smartcase
set autoindent

set cursorline

set nocompatible
filetype plugin on
filetype plugin indent on
syntax on

set listchars=tab:▸\ ,trail:·
set list

set colorcolumn=101

set tabpagemax=100

set spell spelllang=en_us

set makeprg=~/bin/omake
"nnoremap <Space> :make<CR>
"nnoremap <Space> :!omake<CR>
nnoremap <leader><Space> :suspend<cr>

"Prevent //-comment continuation on enter and O/o
inoremap <expr> <enter> getline('.') =~ '^\s*//' ? '<enter><esc>S' : '<enter>'
nnoremap <expr> O getline('.') =~ '\s*//' ? 'O<esc>S' : 'O'
nnoremap <expr> o getline('.') =~ '\s*//' ? 'o<esc>S' : 'o'

set timeout timeoutlen=200

if has("nvim")
    tnoremap ö <C-\><C-n>
endif

inoremap jk <esc>
inoremap ö <esc>

nnoremap <Up> <nop>
nnoremap <Down> <nop>
nnoremap <Left> <nop>
nnoremap <Right> <nop>
nnoremap : <nop>

inoremap <Up> <nop>
inoremap <Down> <nop>
inoremap <Left> <nop>
inoremap <Right> <nop>
inoremap <esc> <nop>

nnoremap ö :

"This function or mapping doesn't work. Fix it.
function! NumberToggle()
    if(&relativenumber == 1)
        set number norelativenumber
    else
        set relativenumber
    endif
endfunc

nnoremap ä :call NumberToggle()<cr>

autocmd InsertEnter * :set norelativenumber
autocmd InsertLeave * :set relativenumber

autocmd BufRead * :set relativenumber

autocmd FileType c          :iabbrev <buffer> iff if ()<cr>{<cr>}<cr>jk3k$i
autocmd FileType c          :iabbrev <buffer> fors for (size_t i = 0; i < ; ++i)<cr>{<cr>}<cr>jk3k$5hi
autocmd FileType c          :iabbrev <buffer> fori for (int i = 0; i < ; ++i)<cr>{<cr>}<cr>jk3k$5hi
autocmd FileType c          :iabbrev <buffer> while  while ()<cr>{<cr>}<cr>jk3k$i

autocmd FileType c          :iabbr <silent> if if ()<cr>{<cr>}<cr>jk3k$i<Left><C-R>=EatChar('\s')<CR>
autocmd FileType c          :iabbr <silent> printf printf("\n",);jk5hi<c-r>=EatChar('\m\s\<bar>/')<cr>

autocmd FileType c          :iabbr <silent> ocve  octaspire_container_vector_<c-r>=EatChar('\m\s\<bar>/')<cr>
autocmd FileType c          :iabbr <silent> ocus  octaspire_container_utf8_string_<c-r>=EatChar('\m\s\<bar>/')<cr>
autocmd FileType c          :iabbr <silent> oma   octaspire_memory_allocator_<c-r>=EatChar('\m\s\<bar>/')<cr>
autocmd FileType c          :iabbr <silent> os    octaspire_stdio_<c-r>=EatChar('\m\s\<bar>/')<cr>
autocmd FileType c          :iabbr <silent> ODA   OCTASPIRE_DERN_AMALGAMATED_<c-r>=EatChar('\m\s\<bar>/')<cr>
autocmd FileType c          :iabbr <silent> ODAI  OCTASPIRE_DERN_AMALGAMATED_IMPLEMENTATION<c-r>=EatChar('\m\s\<bar>/')<cr>
autocmd FileType c          :iabbr <silent> oda   octaspire_dern_amalgamated_<c-r>=EatChar('\m\s\<bar>/')<cr>
autocmd FileType c          :iabbr <silent> oden  octaspire_dern_environment_<c-r>=EatChar('\m\s\<bar>/')<cr>
autocmd FileType c          :iabbr <silent> ochm  octaspire_container_hash_map_<c-r>=EatChar('\m\s\<bar>/')<cr>
autocmd FileType c          :iabbr <silent> ochme octaspire_container_hash_map_element_<c-r>=EatChar('\m\s\<bar>/')<cr>
autocmd FileType c          :iabbr <silent> ocv   octaspire_container_vector_<c-r>=EatChar('\m\s\<bar>/')<cr>
autocmd FileType c          :iabbr <silent> odlt  octaspire_dern_lexer_token_<c-r>=EatChar('\m\s\<bar>/')<cr>
autocmd FileType c          :iabbr <silent> odvm  octaspire_dern_vm_<c-r>=EatChar('\m\s\<bar>/')<cr>
autocmd FileType c          :iabbr <silent> odva  octaspire_dern_value_<c-r>=EatChar('\m\s\<bar>/')<cr>

autocmd FileType c          :iabbrev <buffer> return return;jki

autocmd FileType scheme     :iabbrev <buffer> let (let (())<cr>)<cr>jk2k$hi
autocmd FileType scheme     :iabbrev <buffer> lambda (lambda () ())jk3hi
autocmd FileType scheme     :iabbrev <buffer> define (define )jki
autocmd FileType scheme     :iabbrev <buffer> set! (set! )jki

nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

highlight Pmenu ctermbg=gray ctermfg=white
highlight PmenuSel ctermbg=blue ctermfg=yellow

"For vim-airline to show up?
set laststatus=2

set backspace=indent,eol,start

set exrc
set secure

set nopaste

autocmd FileType c set cindent
autocmd FileType c set cinoptions=(sw

set ruler

set tags=CTAGS;

set spellfile=~/.vim/spell/en.utf-8.add




"-----------------------------------------------------------------------------
" Below this point code is not by www.octaspire.com
"-----------------------------------------------------------------------------

"This function is copied from
"stackoverflow.com/questions/13634826/vim-show-function-name-in-status-line
"fun! ShowFuncName()
"    echohl ModeMsg
"    echo getline(search("^[^ \t#/]\\{2}.*[^:]\s*$", 'bWn'))
"    echohl None
"endfun
fun! ShowFuncName()
    let lnum = line(".")
    let col  = col(".")
    echohl ModeMsg
    echo getline(search("^[^ \t#/]\\{2}.*[^:]\s*$", 'bW'))
    echohl None
    call search("\\%" . lnum . "l" . "\\%" . col . "c")
endfun
nnoremap <leader>f :call ShowFuncName() <CR>


"From stackoverflow
func EatChar(pat)
    let c = nr2char(getchar(0))
    return (c =~ a:pat) ? '' : c
endfunc

