# Motivation

I'd like to be able to quickly sync the important files that I share across computers
Should be as easy as cloning a git repo and being in business


# Stow

## Terms and Example Command


There are some simple terms to work with stow

1) stow directory - The directory that you run stow from, root of repo is intent
2) package directory - An immediate child directory to the stow directory. Contains directories and files

The simple way to use this at the command line is...

`stow -v -t $HOME -R <package-name>`

Each invocation of stow in this manner will put the dirs and files in the target package into ~/

So,

`stow -v -t $HOME -R emacs` will put ./emacs into ~/.emacs, which is where emacs expects it to be.

In this same way, you can work with other dotfiles. Pretty neat.

## Usage

Run the following to install all known dotfiles locally, from this repo

```bash
scripts/stow-all
```
