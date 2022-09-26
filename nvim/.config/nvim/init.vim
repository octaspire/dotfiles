set nocompatible
filetype plugin indent on
set mouse=a
let mapleader = ','

let c_space_errors = 1

packadd minpac
call minpac#init()

call minpac#add('k-takata/minpac', {'type': 'opt'})
" export FZF_DEFAULT_COMMAND='rg --files'
" export PATH=$PATH:$HOME/Library/Python/3.10/bin:$HOME/.roswell/bin
call minpac#add('junegunn/fzf')
call minpac#add('junegunn/fzf.vim')
call minpac#add('tpope/vim-projectionist')
call minpac#add('tpope/vim-dispatch')
call minpac#add('radenling/vim-dispatch-neovim')
call minpac#add('w0rp/ale')
call minpac#add('mhinz/vim-grepper')
call minpac#add('janko-m/vim-test')
call minpac#add('editorconfig/editorconfig-vim')

let test#strategy = "dispatch"

" This function is from book Modern Vim: Craft Your Development Environment
" with Vim 8 and Neovim:
function! SetupCommandAlias(input, output)
	exec 'cabbrev <expr> '.a:input.' ((getcmdtype() is# ":" && getcmdline() is# "'.a:input.'")'.'?("'.a:output.'") : ("'.a:input.'"))'
endfunction

call SetupCommandAlias("grep", "GrepperGrep")

command! PackUpdate call minpac#update()
command! EditInit   execute "edit ~/.config/nvim/init.vim"

nnoremap <C-p> :<C-u>FZF<CR>

if has('nvim')
  tnoremap <Esc> <C-\><C-n>
  tnoremap <C-v><Esc> <Esc>
  highlight! link TermCursor Cursor
  highlight! TermCursorNC guibg=red guifg=white ctermbg=1 ctermfg=15
endif

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

