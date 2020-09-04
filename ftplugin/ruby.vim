setlocal shiftwidth=2 softtabstop=2 tabstop=2 expandtab
nnoremap <F3> :AsyncRun ruby %<cr>
nnoremap <F4> :!ruby %<cr>
vnoremap <F4> :!ruby<cr>
vnoremap ,r :!ruby<cr>

function! RubyEvalCurrentLine()
  let lnum = line('.')
  if getline(lnum) !~ '# =>'
    execute "normal $a # =>\<ESC>"
  endif
  execute "normal :RubyEval\<CR>"
endfunction

noremap ,er :RubyEval<CR>
noremap ,em :call RubyEvalCurrentLine()<CR>

