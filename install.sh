#!/bin/bash

echo "Download dotfiles from github"
if [ ! -d "$HOME/dotfiles" ]; then
    git clone git://github.com/luneyx/dotfiles.git ~/dotfiles
else
    echo "dotfiles already exist here..."
fi

echo "create neccessary folders"
mkdir -p ~/.vim/colors
mkdir -p ~/.vim/autoload

echo "making symbolic link to the dotfiles"
ln -fs ~/.vim ~/.config/nvim
ln -fs ~/dotfiles/vimrc ~/.vimrc
ln -fs ~/dotfiles/vimrc ~/.config/nvim/init.vim
ln -fs ~/dotfiles/vim-colors/luna.vim ~/.vim/colors/luna.vim
ln -fs ~/dotfiles/vim-colors/luna-term.vim ~/.vim/colors/luna-term.vim
ln -fs ~/dotfiles/gitconfig ~/.gitconfig
ln -fs ~/dotfiles/gitignore ~/.gitignore
ln -fs ~/dotfiles/zshrc ~/.zshrc
ln -fs ~/dotfiles/bullet-train.zsh-theme ~/.oh-my-zsh/themes/bullet-train.zsh-theme

echo "Download plug.vim from github"
curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

echo "Download all the plugin of vim"
vim +PlugInstall +qall
echo "Finished!!!"
