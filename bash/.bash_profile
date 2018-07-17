PATH=$PATH:~/bin:~/node_modules/.bin

export PS1="\[\033[93m\]\W\[\033[m\] $ "
export CLICOLOR=1

# Alias
# -- git --
alias ga='git add'
alias gb='git branch'
alias gbd='git branch -d'
alias gc='git commit'
alias gcm='git commit -m'
alias gch='git checkout'
alias gchd='git checkout develop'
alias gchm='git checkout master'
alias gclr='git checkout -- .'
alias gd='git diff'
alias gdc='git diff --cached'
alias gdt='git difftool'
alias gdtt='gdt -t'
alias gg='git grep'
alias gl='git log'
alias glo='git log --oneline'
alias glog='git log --oneline --graph'
alias gloga='git log --oneline --graph --all'
alias gm='git merge'
alias gmt='git mergetool'
alias gp='git push'
alias gpt='git push origin --tags'
alias gpu='git pull'
alias grm='git rm'
alias grmr='git rm -r'
alias gs='git status'
alias gss='git stash save' # Note, meant to be used with a following msg, eg `gss "What this stash does..."
alias gua='git reset HEAD --'
alias gchangedc='git diff-tree --no-commit-id --name-only -r'
alias gchanged='gchangedc HEAD'
alias gshow='git show' # Note, HEAD@{N} can be used to back trace head
alias gshow2='git diff HEAD^ HEAD' # Note, COMMIT^ COMMIT can be used for any commit's diff from it's predecesor
# DELETE all merged git branches 'git branch --merged | egrep -v "(^\*|master|develop)" | xargs git branch -d'
alias gbD='git branch --merged | egrep -v "(master|develop)" | xargs git branch -d'
alias et='emacsclient -t'
alias emacs='emacsclient -c'

alias reloadprof='source ~/.bash_profile'

alias cpdir='cp -'
# Use with dir1/. dir2
alias rmvol='sudo rmdir /Volumes/'

alias grepf='grep -n --color'
alias grepd='grep -rn --color'
alias grepdc='grep -rn -C 3 --color'

alias envs='env | sort'

# -- clojure and clojurescript --
# NOTE: Eastwood only works on .clj files, doesn't know how to handle namespaced keywords.
alias leb="lein eastwood '{:add-linters [:unused-namespaces] :namespaces [bengal.util] :exclude-linters [:unlimited-use]}'"
alias jcljs='find ./src/salk -type f -name "*" | xargs -n 1 joker --lintcljs'
alias clj-repl='rlwrap java -jar ~/.m2/repository/org/clojure/clojure/1.8.0/clojure-1.8.0.jar'
alias clj-server-repl='java -cp ~/.m2/repository/org/clojure/clojure/1.8.0/clojure-1.8.0.jar -Dclojure.server.repl="{:port 5555 :accept clojure.core.server/repl}" clojure.main'
# Requires a deps.edn with the following, and `brew install clojure` to have been run
# {:deps {org.clojure/clojurescript {:mvn/version "1.10.339"}}}
alias cljs='clj -m cljs.main --repl'
alias cljsh='clj -m cljs.main --help'

# Git Completions
if [ -f /usr/local/etc/bash_completion.d/git-completion.bash ]; then
    source /usr/local/etc/bash_completion.d/git-completion.bash
fi

# Secrets : Stuff I don't want to put in github...
if [ -f ~/.bash_secrets ]; then
    source ~/.bash_secrets
fi
