export PATH=${PATH}:~/.local/bin:~/bin:~/.cargo/bin
export ZSH=${HOME}/.oh-my-zsh


ZSH_THEME="robbyrussell"
plugins=(history git common-aliases sudo tmux emacs docker docker-compose fasd you-should-use zsh-syntax-highlighting zsh-autosuggestions zsh-aliases-exa)
source ${ZSH}/oh-my-zsh.sh 

LAYERS="${HOME}/dotfiles/zsh-layers"

# Generic (ish) Layers
source ${LAYERS}/gcloud.zsh
source ${LAYERS}/kubernetes.zsh
source ${LAYERS}/aws.zsh
source ${LAYERS}/golang.zsh

###############################################################################
# History settings
#
HISTSIZE=5000
HISTFILE=~/.zsh_history
SAVEHIST=100000
unsetopt share_history
unsetopt inc_append_history
setopt append_history
setopt inc_append_history_time
setopt extended_history
setopt longlistjobs

###############################################################################
# fzf & fasd 
#
[[ -f ~/.fzf.zsh ]] && source ~/.fzf.zsh
export FZF_DEFAULT_OPTS="--exact"
alias -g F='| fzf -e'

eval "$(fasd --init auto)"

###############################################################################
# Functions
#
# Extract any given archive file
extract() {
 if [[ -z "$1" ]]; then
    # display usage if no parameters given
    echo "Usage: extract <path/file_name>.<zip|rar|bz2|gz|tar|tbz2|tgz|Z|7z|xz|ex|tar.bz2|tar.gz|tar.xz>"
    echo "       extract <path/file_name_1.ext> [path/file_name_2.ext] [path/file_name_3.ext]"
    return 1
 else
    for n in $@
    do
      if [[ -f "$n" ]] ; then
          case "${n%,}" in
            *.tar.bz2|*.tar.gz|*.tar.xz|*.tbz2|*.tgz|*.txz|*.tar) 
                         tar xvf "$n"       ;;
            *.lzma)      unlzma ./"$n"      ;;
            *.bz2)       bunzip2 ./"$n"     ;;
            *.rar)       unrar x -ad ./"$n" ;;
            *.gz)        gunzip ./"$n"      ;;
            *.zip)       unzip ./"$n"       ;;
            *.z)         uncompress ./"$n"  ;;
            *.7z|*.arj|*.cab|*.chm|*.deb|*.dmg|*.iso|*.lzh|*.msi|*.rpm|*.udf|*.wim|*.xar)
                         7z x ./"$n"        ;;
            *.xz)        unxz ./"$n"        ;;
            *.exe)       cabextract ./"$n"  ;;
            *)
                         echo "extract: '$n' - unknown archive method"
                         return 1
                         ;;
          esac
      else
          echo "'$n' - file does not exist"
          return 1
      fi
    done
 fi
}


###############################################################################
# Compatibility with Emacs vterm.
# See: https://github.com/akermu/emacs-libvterm
#
vterm_printf() {
    if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ]); then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

vterm_cmd() {
    local vterm_elisp
    vterm_elisp=""
    while [ $# -gt 0 ]; do
        vterm_elisp="$vterm_elisp""$(printf '"%s" ' "$(printf "%s" "$1" | sed -e 's|\\|\\\\|g' -e 's|"|\\"|g')")"
        shift
    done
    vterm_printf "51;E$vterm_elisp"
}


ZSH_THEME_TERM_TITLE_IDLE="%~"
