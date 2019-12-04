set nocompatible
filetype off

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" Bundles
" Actual plugins
Plugin 'VundleVim/Vundle.vim'
Plugin 'tpope/vim-fugitive'
Plugin 'kien/ctrlp.vim'
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-repeat'
Plugin 'editorconfig/editorconfig-vim'
Plugin 'gerw/vim-HiLinkTrace'
" Meta plugins
Plugin 'vim-scripts/ingo-library'
Plugin 'vim-scripts/SyntaxRange'
" Syntax highlighting
Plugin 'plasticboy/vim-markdown'
Plugin 'cespare/vim-toml'
Plugin 'rust-lang/rust.vim'
Plugin 'mxw/vim-jsx'
Plugin 'pangloss/vim-javascript'
Plugin 'kchmck/vim-coffee-script'
Plugin 'google/vim-jsonnet'
Plugin 'sirtaj/vim-openscad'
Plugin 'leafgarland/typescript-vim'
Plugin 'beyondmarc/glsl.vim'
Plugin 'vim-scripts/scons.vim'
Plugin 'calviken/vim-gdscript3'
Plugin 'wannesm/wmgraphviz.vim'
Plugin 'sotte/presenting.vim'
Plugin 'ziglang/zig.vim'
Plugin 'dracula/vim', { 'name': 'dracula' }
" /Bundles

call vundle#end()

filetype plugin indent on

set laststatus=2
set t_Co=256

let g:vim_markdown_folding_disabled=1
let g:vim_markdown_frontmatter=1
let g:jsx_ext_required = 0

set encoding=utf-8
set tabstop=4
set shiftwidth=4
set autoindent
set magic " unbreak vim's regex implementation

set number
set scrolloff=3
set sidescroll=3

set ruler
set cc=80
set nowrap

set ignorecase
set smartcase

set splitbelow
set hidden
set notimeout

" Search as you type, highlight results
set incsearch
set showmatch
set hlsearch

" Resize windows and move tabs and such with the mouse
set mouse=a

" Don't litter swp files everywhere
set backupdir=~/.cache
set directory=~/.cache

set nofoldenable
set lazyredraw

set tags=./tags;

set printheader=\

syntax on
let mapleader = "\<space>"
nnoremap \\ :noh<cr> " Clear higlighting
nnoremap <silent> <F5> :let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar>:nohl<CR> " Trim trailing spaces
nnoremap Y y$
nnoremap cc :center<cr>
inoremap <C-c> <ESC>
" Ex mode is fucking dumb
nnoremap Q <Nop>

command Jp e ++enc=euc-jp

" Preferences for various file formats
autocmd FileType c setlocal noet ts=4 sw=4 tw=80
autocmd FileType h setlocal noet ts=4 sw=4 tw=80
autocmd FileType cpp setlocal noet ts=4 sw=4 tw=80
autocmd FileType s setlocal noet ts=4 sw=4
autocmd FileType go setlocal noet ts=4 sw=4
autocmd FileType hy setlocal filetype=lisp
autocmd FileType sh setlocal noet ts=4 sw=4
autocmd BufRead,BufNewFile *.js setlocal et ts=2 sw=2
autocmd FileType html setlocal et ts=2 sw=2
autocmd FileType htmldjango setlocal et ts=2 sw=2
autocmd FileType ruby setlocal et ts=2 sw=2
autocmd FileType scss setlocal et ts=2 sw=2
autocmd FileType yaml setlocal et ts=2 sw=2
autocmd FileType markdown setlocal tw=80 et ts=2 sw=2
autocmd FileType text setlocal tw=80
autocmd FileType meson setlocal noet ts=2 sw=2
autocmd FileType bzl setlocal et ts=2 sw=2
autocmd FileType typescript setlocal et ts=2 sw=2
autocmd FileType python setlocal et ts=4 sw=4
autocmd BufNewFile,BufRead *.ms set syntax=python ts=4 sw=4 noet
autocmd BufNewFile,BufRead *.scd set ts=4 sw=4 noet
autocmd FileType tex hi Error ctermbg=NONE
autocmd FileType mail setlocal noautoindent
augroup filetypedetect
  autocmd BufRead,BufNewFile *mutt-*              setfiletype mail
augroup filetypedetect
  autocmd BufRead,BufNewFile *qutebrowser-editor-* set ts=4 sw=4 et
autocmd BufNewFile,BufRead * if expand('%:t') == 'APKBUILD' | set ft=sh | endif
autocmd BufNewFile,BufRead * if expand('%:t') == 'PKGBUILD' | set ft=sh | endif

set guioptions-=m
set guioptions-=T
set guioptions-=r
set guioptions-=e

nmap <leader>l :set list!<CR>
set listchars=tab:▸\ ,eol:¬,space:.

syntax enable


" Transparent editing of gpg encrypted files.
" By Wouter Hanegraaff
augroup encrypted
  au!
  autocmd BufReadPre,FileReadPre *.gpg set viminfo=
  autocmd BufReadPre,FileReadPre *.gpg set noswapfile noundofile nobackup
  autocmd BufReadPre,FileReadPre *.gpg set bin
  autocmd BufReadPre,FileReadPre *.gpg let ch_save = &ch|set ch=2
  autocmd BufReadPost,FileReadPost *.gpg '[,']!gpg --decrypt 2> /dev/null
  autocmd BufReadPost,FileReadPost *.gpg set nobin
  autocmd BufReadPost,FileReadPost *.gpg let &ch = ch_save|unlet ch_save
  autocmd BufReadPost,FileReadPost *.gpg execute ":doautocmd BufReadPost " . expand("%:r")
  autocmd BufWritePre,FileWritePre *.gpg '[,']!gpg --default-recipient-self -ae 2>/dev/null
  autocmd BufWritePost,FileWritePost *.gpg u
augroup END

let g:presenting_top_margin = 2

" set printdevice=EPSON_LX-350
