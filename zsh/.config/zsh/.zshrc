# My zshrc.

# Load some color.
autoload -U colors && colors

# The prompt.
setopt PROMPT_SUBST
PS1='%{$fg[red]%}%(?..X %? )%{$fg[green]%}%n %{$fg[cyan]%}%1~ %{$fg[yellow]%}$(parse_git_branch)%{$reset_color%}$ '

parse_git_branch() {
    git branch 2> /dev/null | sed -e '/^[^*]/d;s/* /[/;s/$/] /'
}

# Automatic cd.
setopt autocd

# Autocompletion.
autoload -U compinit
zstyle ':completion:*' menu select
zmodload zsh/complist
compinit
_comp_options+=(globdots)

# Vi-mode.
bindkey -v
export KEYTIMEOUT=1

bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history
bindkey -v '^?' backward-delete-char

# Aliases
alias c="clear"
alias q="exit"
alias ll="ls -laFoh --color=auto"
alias l="ls -lFoh --color=auto"
alias cp="cp -iv"
alias mv="mv -iv"
alias rm="rm -vI"
alias mkd="mkdir -pv"
alias grep="grep --color=auto"
