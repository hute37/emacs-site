""
" neovim config
" @see: https://github.com/hute37/emacs-site/blob/master/skel/.config/nvim/init.vim
"
" (folding: reduce=zR,zr, more=zM,zm alter=Za,za, nogutter=:set foldcolumn=0)
"

"""{{{ #DOC  ///////////////////////////////////////////////////////////////

""""{{{ ---(nvim update)------------------------------
function _dc_nvim_update()
	" nvim config setup"
	doc <<MD

 * [init.vim](https://github.com/hute37/emacs-site/blob/master/skel/.config/nvim/init.vim)
    
```bash
##
# dots
#

# ===( -> out )=====

cp -pv ~/.config/nvim/init.vim ~/.emacs-site/skel/.config/nvim/ 

cd ~/.emacs-site/
git status
git add . && git commit -m 'nvim config' 
git pull  && git push

#%

# ===( -> in )=====
# cp -pv ~/.emacs-site/skel/.config/nvim/init.vim  ~/.config/nvim/

###
# run
#

nvim --headless +PlugUpgrade +PlugClean +PlugInstall +PlugUpdate +qall

```
MD
endfunction
""""}}}

""""{{{ ---(nvim setup)------------------------------
function _dc_nvim_setup()
	" nvim config setup"
	doc <<MD

 * [init.vim](https://github.com/hute37/emacs-site/blob/master/skel/.config/nvim/init.vim)
    
```bash
##
# apt
#

# sudo apt search neovim
# Y=y
# apt install $Y neovim neovim-qt lua-nvim python3-neovim
# apt install $Y ruby rake bundler ruby-neovim

##
# clean
#

ls -l   ~/.config/nvim
ls -l   ~/.local/share/nvim
rm -rf  ~/.config/nvim
rm -rf  ~/.local/share/nvim

##
# nodots
#

curl -fLo ~/.config/nvim/init.vim --create-dirs \
   https://raw.githubusercontent.com/hute37/emacs-site/master/skel/.config/nvim/init.vim

##
# env
#

cat <'EOF'
export ALTERNATE_EDITOR=""
export EDITOR="emacsclient -t"                  # $EDITOR opens in terminal
export VISUAL="emacsclient -c -a emacs"         # $VISUAL opens in GUI mode	
EOF
	
echo "alias s='less -SRX'"          >> ~/.aliases   
echo "alias e='emacsclient'"        >> ~/.aliases   
echo "alias v='nvim'"               >> ~/.aliases   

echo "[ -f ~/.aliases ] && source ~/.aliases || true"               >> ~/.zsh_aliases   
echo "[ -f ~/.zsh_aliases ] && source ~/.zsh_aliases || true"       >> ~/.zshrc

echo "[ -f ~/.aliases ] && source ~/.aliases || true"               >> ~/.bash_aliases   
echo "[ -f ~/.bash_aliases ] && source ~/.bash_aliases || true"     >> ~/.bashrc

source ~/.aliases

##
# run
#

nvim --headless +PlugUpgrade +PlugInstall +PlugUpdate +qall

```
MD
endfunction
""""}}}

"""""{{{ ---(nvim refs)------------------------------
function _dc_nvim_references()
	" nvim config setup"
	doc <<MD

# VIM References

## Manual

 * [NVim :help](https://neovim.io/doc/user/index.html)

## Guides

 * [Vim Cheat Sheet](https://vim.rtorr.com/)
 * [Vim Primer](https://danielmiessler.com/study/vim/)
    
# VIM Config

## Dotfiles

 * [Derek Taylor](https://gitlab.com/dwt1/dotfiles)

## Plugins

 * [vim-plug](https://github.com/junegunn/vim-plug)

```

```
MD
endfunction
""""}}}

"""}}}


"""{{{ #BASIC ////////////////////////////////////////////////////////////////

set nocompatible              " be iMproved, required
filetype off                  " required

let g:is_win = has('win32') || has('win64')
let g:is_linux = has('unix') && !has('macunix')
let g:is_mac = has('macunix')

let g:nvim_config_root = expand('<sfile>:p:h')

"""{{{ [[shared config]]

"    set runtimepath^=~/.vim runtimepath+=~/.vim/after
"    let &packpath = &runtimepath
"    source ~/.vimrc

"    if exists(':tnoremap')
"        tnoremap <Esc> <C-\><C-n>
"    endif

" @see: https://github.com/jdhao/nvim-config/blob/master/init.vim

""""}}}

"""}}}


"""{{{ #PLUGINS ///////////////////////////////////////////////////////////

if has('nvim')
if empty(glob('~/.local/share/nvim/site/autoload/plug.vim'))
  silent !curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs 
            \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall | source ~/.config/nvim/init.vim
endif
endif

if !has('nvim')
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs 
            \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall | source ~/.config/nvim/init.vim
endif
endif


" ---------------------------------------------------

call plug#begin('~/.local/share/nvim/plugged')

"[[ Layout ]]

Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

"[[ Colors ]]

Plug 'morhetz/gruvbox'
Plug 'tomasr/molokai'
Plug 'fmoralesc/molokayo'
Plug 'ciaranm/inkpot'
Plug 'altercation/vim-colors-solarized'
Plug 'gosukiwi/vim-atom-dark'
Plug 'sonph/onehalf', {'rtp': 'vim/'}

"[[ SCM ]]
Plug 'tpope/vim-fugitive'
""Plug 'jreybert/vimagit'                          " Magit-like plugin for vim

"[[ Files ]]

Plug 'vifm/vifm.vim'                               " Vifm
Plug 'scrooloose/nerdtree'                         " Nerdtree
Plug 'Xuyuanp/nerdtree-git-plugin'				   " Nerdtree git plugin
Plug 'tiagofumo/vim-nerdtree-syntax-highlight'     " Highlighting Nerdtree
Plug 'ryanoasis/vim-devicons'                    " Icons for Nerdtree

"" Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' } " lazy load

"[[ Utility ]]
Plug 'ctrlpvim/ctrlp.vim'

"[[ Junegunn Choi Plugins ]]
Plug 'junegunn/goyo.vim'                           " Distraction-free viewing
Plug 'junegunn/limelight.vim'                      " Hyperfocus on a range
Plug 'junegunn/vim-emoji'                          " Vim needs emojis!

"[[ Commands ]]
if has('nvim')
""Plug 'paretje/nvim-man'
Plug 'jez/vim-superman'
endif
if !has('nvim')
Plug 'vim-utils/vim-man'
endif

"[[ Editing ]]
Plug 'mbbill/undotree'

"[[ Search ]]
Plug 'jremmen/vim-ripgrep'

"[[ Coding ]]
""Plug 'lyuts/vim-rtags'

" @see: https://github.com/ycm-core/YouCompleteMe
"
" cd ~/.local/share/nvim/plugged/YouCompleteMe
" python3 install.py --all
"
"" Plug 'ycm-core/YouCompleteMe'

call plug#end()

"""}}}


"""{{{ #OPTIONS ////////////////////////////////////////////////////////////

" @see: https://youtu.be/n9k9scbTuvQ
" @see: https://gitlab.com/dwt1/dotfiles/-/blob/master/.config/nvim/init.vim

set smartcase
set wildmenu					" Display all matches when tab complete.
set hidden                      " Needed to keep multiple buffers open
syntax enable
let g:rehash256 = 1


""""{{{ #EDITOR

" [[tabs]]

set expandtab                   " Use spaces instead of tabs.
set smarttab                    " Be smart using tabs ;)
set shiftwidth=4                " One tab == four spaces.
set tabstop=4 softtabstop=4     " One tab == four spaces.
set smartindent


" [[search]]
set incsearch
set path+=**					" Searches current directory recursively.

 
" [[undo]]

if has('nvim')
if !isdirectory($HOME."/.local/share/nvim/undo")
    call mkdir($HOME."/.local/share/nvim/undo", "p", 0700)
endif
set undodir=$HOME/.local/share/nvim/undo
endif

if !has('nvim')
if !isdirectory($HOME."/.vim/undo")
    call mkdir($HOME."/.vim/undo", "p", 0700)
endif
set undodir=$HOME/.vim/undo
endif

set undofile

set noswapfile
set nobackup

""""}}}

""""{{{ #ENVIRON

" [[ Clipboard ]]
set clipboard+=unnamedplus

" [[ Terminal ]]
set t_Co=256                    " Set if term supports 256 colors.

" [[ Mouse ]]
"set mouse=nicr

""""}}}

"""}}}


"""{{{ #THEME /////////////////////////////////////////////////////////////

""""{{{ #COLOR

""let g:airline_theme='<theme>' " <theme> is a valid theme name

"" colorscheme gruvbox
"" colorscheme molokai
silent! colorscheme inkpot

if has('gui_running')
    set background=light
else
    set background=dark
endif

"" set background=light " uncomment to use light mode
if executable('rg')
    let g:rg_derive_root='true'
endif

""""}}}

""""{{{ #LAYOUT

" Uncomment to prevent non-normal modes showing in powerline and below powerline.
set noshowmode

""set number relativenumber       " Display line numbers
set nonumber
set foldcolumn=3
highlight FoldColumns ctermbg=none

set colorcolumn=96
highlight ColorColumn ctermbg=0 guibg=lightgrey

set noerrorbells
set nowrap

""""}}}

""""{{{ #GUI

set guioptions-=m  "remove menu bar
set guioptions-=T  "remove toolbar
set guioptions-=r  "remove right-hand scroll bar
set guioptions-=L  "remove left-hand scroll bar

""""}}}

"""}}}


"""{{{ #CUSTOM ////////////////////////////////////////////////////////////

""""{{{ [[ Files ]]

"[[ Files.NERDTree ]]
" Uncomment to autostart the NERDTree
" autocmd vimenter * NERDTree
map <silent> <F3> :NERDTreeToggle<CR>
map <silent> <F4> :NERDTreeFind<CR>

let g:NERDTreeDirArrowExpandable = '►'
let g:NERDTreeDirArrowCollapsible = '▼'
let NERDTreeShowLineNumbers=0
let NERDTreeShowHidden=1
let NERDTreeMinimalUI = 1
let NERDTreeDirArrows = 1
let NERDTreeQuitOnOpen = 1
let g:NERDTreeWinSize=38

"[[ Files.Vifm ]]
map <Leader>vv :Vifm<CR>
map <Leader>vs :VsplitVifm<CR>
map <Leader>sp :SplitVifm<CR>
map <Leader>dv :DiffVifm<CR>
map <Leader>tv :TabVifm<CR>


let g:netrw_browse_split = 2
let g:netrw_banner=0
let g:netrw_winsize = 25


""""}}}

""""{{{ [[ Commands ]]

"[[ Commands.Man ]]

let g:nvim_man_default_target = 'tab'

"[[ Commands.Ctrlp ]]

let g:ctrlp_use_caching = 0
"" let g:ctrlp_user_command = ['.git/', 'git --git-dir=%s.git ls-files -oc ']

""""}}}

""""}}}


"""{{{ #KEYMAP ////////////////////////////////////////////////////////////

let mapleader=' '

" @see: https://vim.fandom.com/wiki/Avoid_the_escape_key
nnoremap <C-space> a
imap <C-space> <Esc>
if exists(':tnoremap')
tnoremap <C-space>  <C-\><C-n>
endif
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
if exists(':tnoremap')
tnoremap <F8>  <C-\><C-n>:tabnext<CR>
tnoremap <F7>  <C-\><C-n>:tabprev<CR>
endif
" @see: https://vim.fandom.com/wiki/Switch_between_Vim_window_splits_easily

imap <C-w> <C-o><C-w>
imap <F6> <C-o><C-w>
map <F6> <C-W>w
if exists(':tnoremap')
tnoremap <F6>  <C-\><C-n><C-W>w
endif

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

" Splits and Tabbed Files
set splitbelow splitright

" Remap splits navigation to just CTRL + hjkl
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

" Make adjusing split sizes a bit more friendly
""noremap <silent> <C-Left> :vertical resize +3<CR>
""noremap <silent> <C-Right> :vertical resize -3<CR>
""noremap <silent> <C-Up> :resize +3<CR>
""noremap <silent> <C-Down> :resize -3<CR>

" Change 2 split windows from vert to horiz or horiz to vert
map <Leader>vv <C-w>t<C-w>H
map <Leader>hh <C-w>t<C-w>K

" Removes pipes | that act as seperators on splits
set fillchars+=vert:\ 


nnoremap <esc><esc> :silent! nohls<cr>

nnoremap <A-x> :
nnoremap <leader>x :
nnoremap <leader>u :tabedit ~/.config/nvim/init.vim<CR>
nnoremap <leader>i :source ~/.config/nvim/init.vim<CR>

nnoremap <leader>t :tabedit<cr>:terminal<cr>i
nnoremap <leader>r :split<cr>:wincmd j<cr>:terminal<cr>i
nnoremap <C-F9> :tabedit<cr>:terminal<cr>i
nnoremap <F9> :split<cr>:wincmd j<cr>:terminal<cr>i
"""nnoremap <leader>r :split term://$SHELL<cr>:startinsert<cr>

" @see: https://superuser.com/questions/321547/how-do-i-replace-paste-yanked-text-in-vim-without-yanking-the-deleted-lines/321726#321726
 
" delete without yanking
nnoremap <leader>d "_d
vnoremap <leader>d "_d

" replace currently selected text with default register
" without yanking it
vnoremap <leader>p "_dP

" @see: https://stackoverflow.com/questions/1889596/vim-mappable-unused-shortcut-letters
nmap ,w :w!<CR>
map ,e :e #<CR>
map ,q :q<CR>
map ,k :qa<CR>
map ,, <F6>
map ,. :q<CR>

" function key synonyms
map ,1 <F1>
map ,2 <F2>
map ,3 <F3>
map ,4 <F4>
map ,5 <F5>
map ,6 <F6>
map ,7 <F7>
map ,8 <F8>
map ,9 <F9>
map ,0 <F10>
map ,- <F11>
map ,= <F12>

"imap ,, <ESC>


"""}}}


"""{{{ #MODE ///////////////////////////////////////////////////////////////

" vim: noet sw=4 ts=4 fdm=marker foldcolumn=0

"""}}}
