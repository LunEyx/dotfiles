" ------------------------
"  Name: Vimrc for Linux
"  Author: LunEyx
" ------------------------

" General {{{

" not compatible to vi
set nocompatible
" show coordinate of cursor
set ruler
" show line number on the left
set number
" smartly ignore case when search
set ignorecase
set smartcase
" highlight searching result
set hlsearch
" search during entering
set incsearch
" base on language indent
set cindent
" turn tab to 4 space
set expandtab
set tabstop=4
set softtabstop=4
set shiftwidth=4
" confirm when executing function should not done
set confirm
" backspace can delete indent and 'next line'
set backspace=indent,eol,start
" record last 1024 command
set history=1024
" show status bar
set laststatus=2
" show command inputting
set showcmd
" show current mode
set showmode
" wrap line when too long
set wrap
" do not auto save
set noautowrite
" mouse select is on visual mode
set mouse=v
" file encodings that reading files may have
set fileencodings=utf-8,utf-16,big5,gb2312,gbk,gb18030,euc-jp,euc-kr,latin1
" file encodings that writing files should be
set encoding=utf-8
" remove delay of <ESC>
set timeout
set timeoutlen=1000
set ttimeout
set ttimeoutlen=10
" diff in vertical
set diffopt+=vertical
" split below or split right
set splitbelow
set splitright
" }}}

" GUI {{{
colorscheme luna-term

if has('gui_running')
    colorscheme luna
    set bg=dark
    set guifont=DejaVu_Sans_Mono_Nerd_Font_Complete:h16
endif

syntax on

" Change cursor shape between insert and normal mode in iTerm2.app
if $TERM_PROGRAM =~ "iTerm"
    let &t_SI = "\<Esc>]50;CursorShape=1\x7" " Vertical bar in insert mode
    let &t_SR = "\<Esc>]50;CursorShape=2\x7" " Underline in replace mode
    let &t_EI = "\<Esc>]50;CursorShape=0\x7" " Block in normal mode
endif
" }}}

" Mapping {{{
let mapleader=","

" <F3> <F4> reserved for compile and run the program

" remove trailling white space
nnoremap <silent> <F5> :let _s=@/ <Bar> :%s/\s\+$//e <Bar> :let @/=_s <Bar> :nohl <Bar> :unlet _s <CR>

" <F6> not used yet
nmap <F7> :AsyncRun<space>
nmap <F8> :TagbarToggle<cr>
nmap <F9> :NERDTreeToggle<cr>
nmap <F10> :call asyncrun#quickfix_toggle(10)<CR>

nmap <C-j> <C-w>j
nmap <C-k> <C-w>k
nmap <C-h> <C-w>h
nmap <C-l> <C-w>l

nmap <Leader>tn :tabnew<cr>
nmap <Leader>tc :tabclose<cr>
nmap <Leader>th :tabp<cr>
nmap <Leader>tl :tabn<cr>

nmap <Leader>sh :split<cr>
nmap <Leader>sv :vsplit<cr>
nmap <Leader>sa <C-w><
nmap <Leader>sd <C-w>>
nmap <Leader>ss <C-w>-
nmap <Leader>sw <C-w>+

if has("nvim")
    nmap <Leader>sth :split<space>term://zsh<cr>i
    nmap <Leader>stv :vsplit<space>term://zsh<cr>i
else
    nmap <Leader>sth :terminal<cr>
    nmap <Leader>stv :vsplit<cr>:terminal<cr><C-w>k:q<cr>
endif
" }}}

" Plugin {{{
" Specify a directory for plugins
" - For Neovim: ~/.local/share/nvim/plugged
" - Avoid using standard Vim directory names like 'plugin'
call plug#begin('~/.vim/plugged')

" Make sure you use single quotes

" Fugitive {{{
Plug 'tpope/vim-fugitive'
" }}}

" Nerdtree {{{
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }
Plug 'Xuyuanp/nerdtree-git-plugin'
" autocmd vimenter * NERDTree
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
let g:NERDTreeWinPos = "right"
let g:NERDTreeDirArrowExpandable = '+'
let g:NERDTreeDirArrowCollapsible = '~'
" }}}

" NERD Commenter {{{
Plug 'scrooloose/nerdcommenter'
let g:NERDSpaceDelims=1
" }}}

" Syntastic {{{
Plug 'scrooloose/syntastic'
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_cpp_check_header = 1
let g:syntastic_cpp_compiler = "g++"
let g:syntastic_cpp_compiler_options = "-Wall -Wextra -Werror -std=c++14"
let g:syntastic_cpp_include_dirs = ["/usr/local/lib", "/usr/local/include", "../src"]
" }}}

" Surround {{{
Plug 'tpope/vim-surround'
" }}}

" Airline {{{
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
let g:airline#extensions#tabline#enabled = 1
let g:airline_theme='luna'
if !exists('g:airline_symbols')
    let g:airline_symbols = {}
endif
let g:airline_left_sep = ''
let g:airline_left_alt_sep = ''
let g:airline_right_sep = ''
let g:airline_right_alt_sep = ''
let g:airline_symbols.branch = ''
let g:airline_symbols.readonly = ''
let g:airline_symbols.linenr = ''
" }}}

" TagBar {{{
Plug 'majutsushi/tagbar'
" for ruby, delete if you do not need
let g:tagbar_type_ruby = {
    \ 'kinds' : [
        \ 'm:modules',
        \ 'c:classes',
        \ 'd:describes',
        \ 'C:contexts',
        \ 'f:methods',
        \ 'F:singleton methods'
    \ ]
\ }
" }}}

" GitGutter {{{
Plug 'airblade/vim-gitgutter'
" }}}

" Easy Align {{{
Plug 'junegunn/vim-easy-align'
" }}}

" Easy Motion {{{
Plug 'easymotion/vim-easymotion'
" }}}

" SuperTab {{{
Plug 'ervandew/supertab'
" }}}

" Snippets {{{
Plug 'SirVer/ultisnips' | Plug 'honza/vim-snippets'

" better key bindings for UltiSnipsExpandTrigger
let g:UltiSnipsExpandTrigger = "<C-j>"
let g:UltiSnipsJumpForwardTrigger = "<C-j>"
let g:UltiSnipsJumpBackwardTrigger = "<C-k>"
let g:UltiSnipsListSnippets = "<C-l>"
" }}}

" Swift {{{
Plug 'bumaociyuan/vim-swift', { 'for': 'swift' }
let g:syntastic_swift_checkers = ['swiftpm', 'swiftlint']
" }}}

" Markdown {{{
Plug 'plasticboy/vim-markdown', { 'for': 'markdown' }
let g:vim_markdown_folding_disabled = 1
" }}}

" Markdown Preview with mathjax support {{{
Plug 'iamcco/mathjax-support-for-mkdp', { 'for': 'markdown'}
Plug 'iamcco/markdown-preview.vim', { 'for': 'markdown' }
let g:mkdp_path_to_chrome = "open -a Google\\ Chrome"
let g:mkdp_auto_start = 1
" }}}

" LimeLight {{{
Plug 'junegunn/limelight.vim', { 'on': 'Limelight' }
let g:limelight_conceal_ctermfg = 242
" }}}

" GoYo {{{
Plug 'junegunn/goyo.vim', { 'on': 'Goyo' }
" }}}

" Asyncrun {{{
Plug 'skywind3000/asyncrun.vim'
let g:asyncrun_bell = 1
" }}}

" WindowSwap {{{
Plug 'wesQ3/vim-windowswap'
" }}}

" Initialize plugin system
call plug#end()
" }}}

" Filetype {{{

" .vim {{{
augroup ft_vim
    autocmd!
    autocmd Filetype vim setlocal foldmethod=marker
    autocmd Filetype vim setlocal foldenable
augroup END
" }}}

" .c {{{
augroup ft_c
    autocmd!
    autocmd Filetype c nnoremap <F3> :AsyncRun clang -Wall -Wextra -pedantic -o %< %<cr>
    autocmd Filetype c nnoremap <F4> :!./%:r<cr>
augroup END
" }}}

" .cpp {{{
augroup ft_cpp
    autocmd!
    autocmd Filetype cpp nnoremap <F3> :AsyncRun clang++ -Wall -Wextra -Werror -std=c++14 -o %< %<cr>
    autocmd Filetype cpp nnoremap <F4> :!./%:r<cr>
augroup END
" }}}

" .py {{{
augroup ft_py
    autocmd!
    autocmd Filetype py nnoremap <F3> :AsyncRun python %<cr>
    autocmd Filetype py nnoremap <F4> :!python %<cr>
augroup END
" }}}

" .rb {{{
augroup ft_rb
    autocmd!
    autocmd Filetype rb nnoremap <F3> :AsyncRun ruby %<cr>
    autocmd Filetype rb nnoremap <F4> :!ruby %<cr>
augroup END
" }}}

" .swift {{{
augroup ft_swift
    autocmd!
    autocmd Filetype swift nnoremap <F3> :AsyncRun swiftc %<cr>
    autocmd Filetype swift nnoremap <F4> :!swift %<cr>
augroup END
" }}}

" }}}

" neovim {{{
if has("nvim")
    tmap <C-j> <C-\><C-n><C-j>
    tmap <C-k> <C-\><C-n><C-k>
    tmap <C-h> <C-\><C-n><C-h>
    tmap <C-l> <C-\><C-n><C-l>
    let g:python_host_prog="/usr/bin/python"
    let g:python3_host_prog="/usr/local/bin/python3"
endif
" }}}
