" Octaspire dotfiles - Various configuration files
" Copyright 2017, 2018, 2020, 2022  www.octaspire.com
"
" Licensed under the Apache License, Version 2.0 (the "License");
" you may not use this file except in compliance with the License.
" You may obtain a copy of the License at
"
"    http://www.apache.org/licenses/LICENSE-2.0
"
" Unless required by applicable law or agreed to in writing, software
" distributed under the License is distributed on an "AS IS" BASIS,
" WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
" See the License for the specific language governing permissions and
" limitations under the License.
"
" Pair with the following additions into .bashrc:
" -----------------------------------------------
" export FZF_DEFAULT_COMMAND='rg --files'
" export PATH=$PATH:$HOME/Library/Python/3.10/bin:$HOME/.roswell/bin
" export VISUAL="nvr -cc split --remote-wait +'set bufhidden=wipe'"
"
" install nvr using:
" pip3 install neovim-remote
"
" and remove any possible editor names from .gitconfig, so that
" git uses $VISUAL as the default commit editor.
"
" Server for vlime can be started with command:
" sbcl --load $HOME/.config/nvim/pack/minpac/start/vlime/lisp/start-vlime.lisp
set nocompatible
filetype plugin indent on
set mouse=a

let c_space_errors = 1
set listchars=tab:>-,trail:\\u2423
set list
set tabstop=2
set shiftwidth=2
set expandtab

packadd minpac
call minpac#init()

call minpac#add('k-takata/minpac', {'type': 'opt'})
call minpac#add('junegunn/fzf')
call minpac#add('junegunn/fzf.vim')
call minpac#add('tpope/vim-projectionist')
call minpac#add('tpope/vim-dispatch')
call minpac#add('radenling/vim-dispatch-neovim')
call minpac#add('w0rp/ale')
call minpac#add('mhinz/vim-grepper')
call minpac#add('janko-m/vim-test')
call minpac#add('editorconfig/editorconfig-vim')
call minpac#add('morhetz/gruvbox')
call minpac#add('tpope/vim-obsession')
call minpac#add('vlime/vlime')

let test#strategy = "dispatch"

command! PackUpdate call minpac#update()
command! EditInit   execute "edit ~/.config/nvim/init.vim"

nnoremap <C-p> :<C-u>FZF<CR>

if has('nvim')
  tnoremap <Esc> <C-\><C-n>
  tnoremap <C-v><Esc> <Esc>
  highlight! link TermCursor Cursor
  highlight! TermCursorNC guibg=red guifg=white ctermbg=1 ctermfg=15
  let $NVIM_TUI_ENABLE_TRUE_COLOR=1
endif

set bg=dark
let g:gruvbox_contrast_dark = "hard"
let g:gruvbox_transparent_bg = '1'
colorscheme gruvbox

" Grepping
nnoremap <Leader>g :Grepper -tool rg<CR>

let g:ale_linters = {'lisp': ['sblint']}

let g:grepper = {}
let g:grepper.tools = ['grep', 'git', 'rg']

" Search for the current word
nnoremap <Leader>* :Grepper -cword -noprompt<CR>

" Search for the current selection
nmap gs <plug>(GrepperOperator)
xmap gs <plug>(GrepperOperator)

nmap <silent> [W <Plug>(ale_first)
nmap <silent> [w <Plug>(ale_previous)
nmap <silent> ]w <Plug>(ale_next)
nmap <silent> ]W <Plug>(ale_last)

