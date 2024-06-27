export PATH=~/.bin:~/.local/bin:$PATH
export LC_ALL=ru_RU.UTF-8
export LANG=ru_RU.UTF-8
export LANGUAGE=ru_RU.UTF-8

export NAME='Anton Konyahin'
export EMAIL='me@konyahin.xyz'

export EDITOR='nvi'
export ALTERNATE_EDITOR='emacs -nw'
export PAGER=less
export BROWSER=open
export PASSWORD_STORE_DIR="$HOME/data/pass"

alias z=zathura
alias e='$EDITOR'
alias em='$ALTERNATE_EDITOR'
alias v=nview
alias nb='newsboat && bookmark count'
alias m='mutt && isync'
alias isync='mbsync -c ~/.config/.mbsyncrc -a && mfilter ~/data/mail/fastmail'
alias l='ls -p -A'
alias lr='l -R'

alias evi='$EDITOR ~/.nexrc'
alias enb='$EDITOR ~/.newsboat/config'
alias enbu='$EDITOR ~/.newsboat/urls'

alias ef='$EDITOR $(find . | fzf)'
alias et='$EDITOR -t $(cat tags | awk "{print \$1}" | fzf)'
alias edot='$EDITOR "$(find ~/dotfiles -type f ! -path "*.git/*" | fzf)"'

alias gpush='git remote | xargs -n 1 git push'

fzf-in () {
    found=$(find . -not -path "*.git*" | fzf)
    [ -d "$found" ] && cd "$found" && return
    [ -f "$found" ] && $EDITOR "$found" && return
}
alias /=fzf-in
alias //='cd ~/; /'

me () {
    $EDITOR $(make 2>&1 | grep '^.*\.[ch]:' | fzf | awk -F: '{printf "-c %s %s\n", $2, $1}')
}

jp () {
    projects=$(find ~/project/ -type d -maxdepth 1 -mindepth 1 ! -name konyahin)
    personal=$(find ~/project/konyahin -type d -maxdepth 1 -mindepth 1)
    selected=$(echo "$projects\n$personal" | fzf)
    [ -n "$selected" ] && cd "$selected"
}

cheat () {
    curl "cheat.sh/$1" | less -r
}
