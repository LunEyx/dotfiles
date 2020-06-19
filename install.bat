ECHO OFF
COPY /Y vimrc %APPDATA%\..\Local\nvim\init.vim
XCOPY /Y vim-colors %APPDATA%\..\Local\nvim\colors
PAUSE
