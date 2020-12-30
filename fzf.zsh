# Setup fzf
# ---------
if [[ ! "$PATH" == */home/athompson/.fzf/bin* ]]; then
  export PATH="$PATH:/home/athompson/.fzf/bin"
fi

# Auto-completion
# ---------------
[[ $- == *i* ]] && source "/home/athompson/.fzf/shell/completion.zsh" 2> /dev/null

# Key bindings
# ------------
source "/home/athompson/.fzf/shell/key-bindings.zsh"

