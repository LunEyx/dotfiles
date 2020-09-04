if has('win32') || has('win64')
    nnoremap <F3> :AsyncRun clang++ -Wall -Wextra -std=c++14 -o %<.exe %<cr>
    nnoremap <F4> :AsyncRun %<<cr>
else
    nnoremap <F3> :AsyncRun clang++ -Wall -Wextra -std=c++14 -o %< %<cr>
    nnoremap <F15> :AsyncRun clang++ -Wall -Wextra -Werror -std=c++14 -o %< %<cr>
    nnoremap <F4> :AsyncRun ./%:r<cr>
endif

