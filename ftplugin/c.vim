setlocal tabstop=4
setlocal shiftwidth=4
setlocal softtabstop=4

setlocal foldmethod=syntax
setlocal foldnestmax=1

if (&ft != 'c')
    finish
endif

if has('win32') || has('win64')
    nnoremap <F3> :AsyncRun clang -o %<.exe %<cr>
    nnoremap <F4> :!%:r<cr>
else
    nnoremap <F3> :AsyncRun clang -o %< %<cr>
    nnoremap <F4> :!./%:r<cr>
endif

