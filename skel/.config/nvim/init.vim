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
colorscheme molokai

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

"" let mapleader=' '

