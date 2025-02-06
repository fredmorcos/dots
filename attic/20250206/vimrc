set runtimepath^=$XDG_CONFIG_HOME/vim
set runtimepath+=$XDG_DATA_HOME/vim
set runtimepath+=$XDG_CONFIG_HOME/vim/after

set packpath^=$XDG_DATA_HOME/vim,$XDG_CONFIG_HOME/vim
set packpath+=$XDG_CONFIG_HOME/vim/after,$XDG_DATA_HOME/vim/after

let g:netrw_home = $XDG_DATA_HOME."/vim"
call mkdir($XDG_DATA_HOME."/vim/spell", 'p')

set backupdir=$XDG_STATE_HOME/vim/backup | call mkdir(&backupdir, 'p')
set directory=$XDG_STATE_HOME/vim/swap   | call mkdir(&directory, 'p')
set undodir=$XDG_STATE_HOME/vim/undo     | call mkdir(&undodir,   'p')
set viewdir=$XDG_STATE_HOME/vim/view     | call mkdir(&viewdir,   'p')

if !has('nvim') | set viminfofile=$XDG_STATE_HOME/vim/viminfo | endif

let &t_ut=''

set softtabstop=2
set expandtab
set laststatus=2
set shiftwidth=2
set tabstop=2
set encoding=utf-8

set number                                     " Show line numbers.
set cursorline                                 " Highlight the current line.
set wildmenu                                   " Show an autocompletion bar at the bottom.
set showmatch                                  " Highlight matching parens and braces.
set hlsearch                                   " Highlight search matches.
set incsearch                                  " Search as characters are entered.
set ignorecase
set smartcase
set autoindent
set ruler
set colorcolumn=100
set listchars=tab:▸\ ,eol:¬                    " Use `:set list` to show invisible characters

set mouse=r

" turn off search highlight with ,SPC
nnoremap <leader><space> :nohlsearch<CR>

filetype plugin indent on
filetype indent on
syntax on
syntax enable

" Disable cursor blinking in GVim
set guicursor+=a:blinkon0

set guifont=Monospace\ 9

" When opening a file, return to the last cursor position in that file
:au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif

set backup
set backupdir=/tmp
set directory=/tmp
set writebackup

if &diff
  " colorscheme seti
  :map n ]c
  :map pp dp
  :map oo do
endif
