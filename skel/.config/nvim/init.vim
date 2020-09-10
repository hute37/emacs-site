"    set runtimepath^=~/.vim runtimepath+=~/.vim/after
"    let &packpath = &runtimepath
"    source ~/.vimrc

"    if exists(':tnoremap')
"        tnoremap <Esc> <C-\><C-n>
"    endif

" @see: https://github.com/jdhao/nvim-config/blob/master/init.vim

"{ Main configurations
let g:is_win = has('win32') || has('win64')
let g:is_linux = has('unix') && !has('macunix')
let g:is_mac = has('macunix')

let g:nvim_config_root = expand('<sfile>:p:h')

" mkdir -p ~/.local/share/nvim/undodir
let g:nvim_undo_dir = '~/.local/share/nvim/undodir'


""
" #OPTIONS
"

" @see: https://youtu.be/n9k9scbTuvQ

set noerrorbells
set tabstop=4 softtabstop=4
set shiftwidth=4
set expandtab
set smartindent
set nowrap
set smartcase
set noswapfile
set nobackup
set undodir=g:nvim_undo_dir
set undofile
set incsearch

set nonumber
set foldcolumn=3
highlight FoldColumns ctermbg=none

set colorcolumn=96
highlight ColorColumn ctermbg=0 guibg=lightgrey


""
" #ENVIRON: session
"

set clipboard+=unnamedplus


""
" #PLUGINS: vim-plug
"


" ---(vim-plug install)------------------------------
function _dc_vimplug_install()
	" vim-plug install"
	doc <<MD
```
 * [vim-plug](https://github.com/junegunn/vim-plug)

```
MD
endfunction

if empty(glob('~/.local/share/nvim/site/autoload/plug.vim'))
  silent !curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs 
            \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall | source ~/.config/nvim/init.vim
endif

" ---------------------------------------------------

call plug#begin('~/.local/share/nvim/plugged')

Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

Plug 'morhetz/gruvbox'
Plug 'tomasr/molokai'
Plug 'fmoralesc/molokayo'
Plug 'ciaranm/inkpot'
Plug 'altercation/vim-colors-solarized'
Plug 'gosukiwi/vim-atom-dark'
Plug 'sonph/onehalf', {'rtp': 'vim/'}


Plug 'jremmen/vim-ripgrep'
Plug 'tpope/vim-fugitive'
Plug 'vim-utils/vim-man'
""Plug 'lyuts/vim-rtags'
Plug 'mbbill/undotree'
Plug 'ctrlpvim/ctrlp.vim'

" @see: https://github.com/ycm-core/YouCompleteMe
"
" cd ~/.local/share/nvim/plugged/YouCompleteMe
" python3 install.py --all
"

"" Plug 'ycm-core/YouCompleteMe'



" NERD tree will be loaded on the first invocation of NERDTreeToggle command
Plug 'scrooloose/nerdtree'
"" Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }


call plug#end()

""let g:airline_theme='<theme>' " <theme> is a valid theme name

"" colorscheme gruvbox
"" colorscheme molokai
colorscheme inkpot

if has('gui_running')
    set background=light
else
    set background=dark
endif

"" set background=light " uncomment to use light mode
if executable('rg')
    let g:rg_derive_root='true'
endif



let g:netrw_browse_split = 2
let g:netrw_banner=0
let g:netrw_winsize = 25

let g:ctrlp_use_caching = 0
"" let g:ctrlp_user_command = ['.git/', 'git --git-dir=%s.git ls-files -oc ']

""
" #KEYMAP
"

let mapleader=' '

" @see: https://vim.fandom.com/wiki/Avoid_the_escape_key
nnoremap <C-space> a
imap <C-space> <Esc>
tnoremap <C-space>  <C-\><C-n>
"" inoremap <silent> <Up> <ESC><Up>
"" inoremap <silent> <Down> <ESC><Down>
" @see: https://vim.fandom.com/wiki/Alternative_tab_navigation

nnoremap th  :tabfirst<CR>
nnoremap tk  :tabnext<CR>
nnoremap tj  :tabprev<CR>
nnoremap tl  :tablast<CR>
nnoremap tt  :tabedit<Space>
nnoremap tn  :tabnext<Space>
nnoremap tm  :tabm<Space>
nnoremap td  :tabclose<CR>

nnoremap <F8>  :tabnext<CR>
nnoremap <F7>  :tabprev<CR>
tnoremap <F8>  <C-\><C-n>:tabnext<CR>
tnoremap <F7>  <C-\><C-n>:tabprev<CR>

" @see: https://vim.fandom.com/wiki/Switch_between_Vim_window_splits_easily

imap <C-w> <C-o><C-w>
imap <F6> <C-o><C-w>
map <F6> <C-W>w
tnoremap <F6>  <C-\><C-n><C-W>w

nnoremap <F5> :buffers<CR>:buffer<Space>
""nnoremap <C-6> :b#

" @see: https://vim.fandom.com/wiki/Short_mappings_for_common_tasks

vmap <cr> y
vmap > >gv
vmap < <gv

"" nnoremap Y y$
nmap <C-W>* <C-W>s*
nmap <C-W># <C-W>s#

" 

map <C-Down> <c-e>
map <C-Up> <c-y>
map <S-Down> j
map <S-Up> k


nnoremap <esc><esc> :silent! nohls<cr>

nnoremap <leader>x :
nnoremap <leader>u :tabedit ~/.config/nvim/init.vim
nnoremap <leader>i :source ~/.config/nvim/init.vim

nnoremap <leader>t :tabedit<cr>:terminal<cr>:startinsert<cr>
nnoremap <leader>r :split<cr>:wincmd j<cr>:terminal<cr>:startinsert<cr>
""nnoremap <leader>r :split term://$SHELL<cr>:startinsert<cr>


