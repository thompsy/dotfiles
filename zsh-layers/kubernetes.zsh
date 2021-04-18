# CLI completion for kubectl/helm
if [ $commands[kubectl] ]; then
  source <(kubectl completion zsh)
fi

if [ $commands[helm] ]; then
  source <(helm completion zsh)
fi

# Load individual kube contexts inot $KUBECONFIG
KCONFDIR=~/.kube
export KUBECONFIG=${KCONFDIR}/dev.config:${KCONFDIR}/prod.config


# Utility
alias k='kubectl'

# Get Cmds
alias kg='kubectl get'
alias kgp='kubectl get pods'
alias kgns='kubectl get namespaces'
alias kgall='kubectl get ingress,service,deployment,pod'
alias kgcj='kuebctl get cronjobs'
alias kgj='kubectl get jobs'
alias kctx=kubectx

# Configuration cmds
alias kuc='kubectl config use-context'
alias ksc='kubectl config set-context "$(kubectl config current-context)"'
alias kns='kubens'

# Networking
alias kpf='kubectl port-forward'
alias kp='kubectl proxy'

