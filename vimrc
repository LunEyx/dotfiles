" ------------------------
"  Name: Vimrc for Linux
"  Author: LunEyx
" ------------------------

" General {{{

" not compatible to vi
set nocompatible
" not show coordinate of cursor
set noruler
" show relative line number on the left
set relativenumber
" smartly ignore case when search
set ignorecase
set smartcase
" highlight searching result
set hlsearch
" search during entering
set incsearch
" base on language indent
set cindent
" turn tab to 2 space
set expandtab
set tabstop=2
set softtabstop=2
set shiftwidth=2
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
" system clipboard synconize with vim
set clipboard=unnamed
" show partial off-screen results in a preview window
set inccommand=split
" }}}

" GUI {{{
set background=light
colorscheme PaperColor
if has('win32') || has('win64')
    if has('gui_running')
        colorscheme PaperColor
        " disable
        set guioptions-=T
        set guioptions-=m
        set guioptions-=L
        set guioptions-=r
        set guioptions-=b
        " using internal tab instead of gui format
        set guioptions-=e
        set guifont=DejaVu\ Sans\ Mono\ for\ Powerline:h14,DejaVu\ Sans\ Mono:h14,Consolas:h15
    " elseif has('nvim')
        " colorscheme PaperColor
    endif
" elseif has('gui_vimr')
    " colorscheme PaperColor
elseif has('gui_running')
    set guifont=DejaVu_Sans_Mono_Nerd_Font_Complete:h16
else
endif

syntax on

" Change cursor shape between insert and normal mode in iTerm2.app
if $TERM_PROGRAM =~ "iTerm"
    let &t_SI = "\<Esc>]50;CursorShape=1\x7" " Vertical bar in insert mode
    let &t_SR = "\<Esc>]50;CursorShape=2\x7" " Underline in replace mode
    let &t_EI = "\<Esc>]50;CursorShape=0\x7" " Block in normal mode
endif

function! LightTheme()
    set background=light
    colorscheme PaperColor
    AirlineTheme papercolor
endfunction

function! DarkTheme()
    set background=dark
    colorscheme luna
    AirlineTheme luna
endfunction
" }}}

" Mapping {{{
let mapleader=","
let maplocalleader=","

" <F3> <F4> reserved for compile and run the program

" remove trailling white space
nnoremap <silent> <F5> :let _s=@/ <Bar> :%s/\s\+$//e <Bar> :let @/=_s <Bar> :nohl <Bar> :unlet _s <CR>

" <F6> not used yet
nmap <F7> :AsyncRun<space>
nmap <F8> :NERDTreeToggle<cr>
nmap <F9> :TagbarToggle<cr>
nmap <F10> :call asyncrun#quickfix_toggle(10)<CR>

function! GetBufferList()
  redir =>buflist
  silent! ls!
  redir END
  return buflist
endfunction

function! ToggleList(bufname, pfx)
  let buflist = GetBufferList()
  for bufnum in map(filter(split(buflist, '\n'), 'v:val =~ "'.a:bufname.'"'), 'str2nr(matchstr(v:val, "\\d\\+"))')
    if bufwinnr(bufnum) != -1
      exec(a:pfx.'close')
      return
    endif
  endfor
  if a:pfx == 'l' && len(getloclist(0)) == 0
      echohl ErrorMsg
      echo "Location List is Empty."
      return
  endif
  let winnr = winnr()
  exec(a:pfx.'open')
  if winnr() != winnr
    wincmd p
  endif
endfunction

nmap <silent> <leader><leader>l :call ToggleList("Location List", 'l')<CR>
nmap <silent> <leader>ll :call ToggleList("Quickfix List", 'c')<CR>

nmap <C-j> <C-w>j
nmap <C-k> <C-w>k
nmap <C-h> <C-w>h
nmap <C-l> <C-w>l

nmap <Leader>tn :tabnew<cr>
nmap <Leader>tc :tabclose<cr>
nmap <Leader>th :tabp<cr>
nmap <Leader>tl :tabn<cr>

nmap <Leader>bh :bp<cr>
nmap <Leader>bl :bn<cr>
nmap <Leader>bb :Buffers<cr>

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

" Appearance {{{

" Airline {{{
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
let g:airline#extensions#tabline#enabled = 1
let g:airline_theme='papercolor'
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

let g:airline#extensions#default#section_truncate_width = {
  \ 'b': 79,
  \ 'x': 60,
  \ 'y': 88,
  \ 'z': 75,
  \ 'warning': 60,
  \ 'error': 50,
  \ }
" }}}

" }}}

" Completion {{{

" ALE {{{
Plug 'w0rp/ale'
" Write this in your vimrc file
let g:ale_lint_on_text_changed = 'never'
let g:ale_linters = {
            \   'c': ['clang'],
            \   'cpp': ['clang'],
            \   'beancount': [],
            \   'tex': [],
            \}
let g:ale_cpp_clang_options_origin = '-Wall -Wextra -Wno-unused-parameter -std=c++14 -Isrc -Iinlcude'
let g:ale_cpp_clang_options = '-Wall -Wextra -Wno-unused-parameter -std=c++14 -Isrc -Iinlcude'
let g:ale_c_clang_options = '-Wall -Wextra -I src'
" }}}

" NCM2 {{{
Plug 'ncm2/ncm2'
Plug 'roxma/nvim-yarp'

auto BufEnter * call ncm2#enable_for_buffer()
set completeopt=noinsert,menuone,noselect
" set shortmess+=c
inoremap <expr> <CR> (pumvisible() ? "\<C-y>\<CR>" : "\<CR>")
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"

Plug 'ncm2/ncm2-abbrfuzzy'
Plug 'ncm2/ncm2-bufword'
Plug 'ncm2/ncm2-path'
Plug 'ncm2/ncm2-ultisnips'
" }}}

" Snippets {{{
Plug 'SirVer/ultisnips' | Plug 'honza/vim-snippets'

" better key bindings for UltiSnipsExpandTrigger
let g:UltiSnipsExpandTrigger = "<C-j>"
let g:UltiSnipsJumpForwardTrigger = "<C-j>"
let g:UltiSnipsJumpBackwardTrigger = "<C-k>"
let g:UltiSnipsListSnippets = "<C-l>"
let g:UltiSnipsRemoveSelectModeMappings = 0
" }}}

" }}}

" Editor Level Support {{{

" Nerdtree {{{
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }
Plug 'Xuyuanp/nerdtree-git-plugin'
" autocmd vimenter * NERDTree
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
let g:NERDTreeDirArrowExpandable = '+'
let g:NERDTreeDirArrowCollapsible = '~'
" }}}

" NERD Commenter {{{
Plug 'scrooloose/nerdcommenter'
let g:NERDSpaceDelims=1
" }}}

" Surround {{{
Plug 'tpope/vim-surround'
" }}}

" TagBar {{{
Plug 'majutsushi/tagbar', { 'on': 'TagbarToggle' }
" }}}

" Easy Align {{{
Plug 'junegunn/vim-easy-align'
" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)
" }}}

" Easy Motion {{{
Plug 'easymotion/vim-easymotion'
" }}}

" Markdown Preview with mathjax support {{{
Plug 'iamcco/mathjax-support-for-mkdp', { 'for': 'markdown'}
Plug 'iamcco/markdown-preview.vim', { 'for': 'markdown' }
let g:mkdp_path_to_chrome = "open -a Google\\ Chrome"
let g:mkdp_auto_start = 0
" }}}

" LimeLight {{{
Plug 'junegunn/limelight.vim', { 'on': 'Limelight' }
let g:limelight_conceal_ctermfg = 242
" }}}

" GoYo {{{
Plug 'junegunn/goyo.vim', { 'on': 'Goyo' }
" }}}

" Asyncrun {{{
Plug 'skywind3000/asyncrun.vim', { 'on': 'AsyncRun' }
let g:asyncrun_bell = 1
" }}}

" WindowSwap {{{
Plug 'wesQ3/vim-windowswap'
" }}}

" FZF {{{
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
" }}}

" Speed Dating {{{
Plug 'tpope/vim-speeddating'
" }}}

" Universal Text Linking {{{
Plug 'vim-scripts/utl.vim'
let g:utl_cfg_hdl_scm_http_system = "silent !open '%u'"
let g:utl_cfg_hdl_scm_http = g:utl_cfg_hdl_scm_http_system
" }}}

" Syntax Range {{{
Plug 'vim-scripts/SyntaxRange'
" }}}

" }}}

" Git {{{

" Fugitive {{{
Plug 'tpope/vim-fugitive'
" }}}

" GitGutter {{{
Plug 'airblade/vim-gitgutter'
" }}}

" vimagit {{{
Plug 'jreybert/vimagit'
" }}}

" }}}

" Language {{{

" Swift {{{
Plug 'bumaociyuan/vim-swift', { 'for': 'swift' }
" }}}

" Markdown {{{
Plug 'plasticboy/vim-markdown', { 'for': 'markdown' }
let g:vim_markdown_folding_disabled = 1
" }}}

" Beancount {{{
Plug 'nathangrigg/vim-beancount', { 'for': 'beancount' }
let b:beancount_root="~/Beancount/journal.beancount"
" }}}

" GLSL {{{
Plug 'tikhomirov/vim-glsl', { 'for': 'glsl' }
" }}}

" MIPS {{{
Plug 'harenome/vim-mipssyntax', { 'for': 'mips' }
" }}}

" Haml {{{
Plug 'tpope/vim-haml', { 'for': 'haml' }
" }}}

" Rails {{{
Plug 'tpope/vim-rails', { 'for': 'ruby' }
" }}}

" Ruby {{{
Plug 'vim-ruby/vim-ruby', { 'for': 'ruby' }
" }}}

" }}}

" Language Level Support {{{

" Latex live preview {{{
Plug 'xuhdev/vim-latex-live-preview', { 'for': 'tex' }
let g:livepreview_previewer = 'open -a Skim'
let g:livepreview_engine = 'xelatex' . ' '
" }}}

" Emmet {{{
Plug 'mattn/emmet-vim', { 'for': 'html' }
" }}}

" Ruby-Eval {{{
Plug 'kmdsbng/vim-ruby-eval', { 'for': 'ruby' }
noremap ,er :RubyEval<CR>
noremap ,em $a # =><ESC>:RubyEval<CR>
" }}}

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
    if has('win32') || has('win64')
        autocmd Filetype c nnoremap <F3> :AsyncRun clang -o %<.exe %<cr>
        autocmd Filetype c nnoremap <F4> :!%:r<cr>
    else
        autocmd Filetype c nnoremap <F3> :AsyncRun clang -o %< %<cr>
        autocmd Filetype c nnoremap <F4> :!./%:r<cr>
    endif
augroup END
" }}}

" .cpp {{{
augroup ft_cpp
    autocmd!
    if has('win32') || has('win64')
        autocmd Filetype cpp nnoremap <F3> :AsyncRun clang++ -Wall -Wextra -std=c++14 -o %<.exe %<cr>
        autocmd Filetype cpp nnoremap <F4> :AsyncRun %<<cr>
    else
        autocmd Filetype cpp nnoremap <F3> :AsyncRun clang++ -Wall -Wextra -std=c++14 -o %< %<cr>
        autocmd Filetype cpp nnoremap <F15> :AsyncRun clang++ -Wall -Wextra -Werror -std=c++14 -o %< %<cr>
        autocmd Filetype cpp nnoremap <F4> :AsyncRun ./%:r<cr>
    endif
augroup END
" }}}

" .html {{{
augroup ft_html
    autocmd!
    autocmd Filetype html setlocal shiftwidth=2 softtabstop=2 tabstop=2 expandtab
    autocmd Filetype html nnoremap <F3> :AsyncRun open %<cr>
    autocmd Filetype html nnoremap <F4> :!open %<cr>
augroup END
" }}}

" .java {{{
augroup ft_java
    autocmd!
    autocmd Filetype java nnoremap <F3> :AsyncRun javac %<cr>
    autocmd Filetype java nnoremap <F4> :AsyncRun java %< <cr>
augroup END
" }}}

" .py {{{
augroup ft_py
    autocmd!
    autocmd Filetype python nnoremap <F3> :AsyncRun python %<cr>
    autocmd Filetype python nnoremap <F4> :AsyncRun python %<cr>
augroup END
" }}}

" .rb {{{
augroup ft_rb
    autocmd!
    autocmd Filetype ruby setlocal shiftwidth=2 softtabstop=2 tabstop=2 expandtab
    autocmd Filetype ruby nnoremap <F3> :AsyncRun ruby %<cr>
    autocmd Filetype ruby nnoremap <F4> :!ruby %<cr>
    autocmd Filetype ruby vnoremap <F4> :!ruby<cr>
    autocmd Filetype ruby vnoremap ,r :!ruby<cr>
augroup END
" }}}

" .erb {{{
augroup ft_erb
    autocmd!
    autocmd Filetype eruby setlocal shiftwidth=2 softtabstop=2 tabstop=2 expandtab
augroup END
" }}}

" .swift {{{
augroup ft_swift
    autocmd!
    autocmd Filetype swift nnoremap <F3> :AsyncRun swiftc %<cr>
    autocmd Filetype swift nnoremap <F4> :!swift %<cr>
augroup END
" }}}

" .bean/.beancount {{{
augroup ft_beancount
    autocmd!
    autocmd Filetype beancount nnoremap <Leader><Leader>a :AlignCommodity<cr>
    autocmd Filetype beancount vnoremap <Leader><Leader>a :'<,'>AlignCommodity<cr>
    autocmd Filetype beancount nnoremap <F3> :!bean-check journal.beancount<cr>
    autocmd Filetype beancount nnoremap <F4> :AsyncRun bean-web %<cr>
augroup END
" }}}

" .tex {{{
let g:tex_flavor = "latex"
" }}}

" }}}

" neovim {{{
if has("nvim")
    tmap <C-j> <C-\><C-n><C-j>
    tmap <C-k> <C-\><C-n><C-k>
    tmap <C-h> <C-\><C-n><C-h>
    tmap <C-l> <C-\><C-n><C-l>
    let g:python_host_prog="/usr/local/bin/python"
    let g:python3_host_prog="/usr/local/bin/python3"
endif
" }}}
