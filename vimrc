let &t_ut=''

set number          " Show line numbers.
set cursorline      " Highlight the current line.
set wildmenu        " Show an autocompletion bar at the bottom.
set showmatch       " Highlight matching parens and braces.
set hlsearch        " Highlight search matches.
set incsearch       " Search as characters are entered.
set ignorecase
set smartcase
set autoindent
set ruler
set colorcolumn=100

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
  colorscheme sienna
endif
