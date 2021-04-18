###############################################################################
# Go
#
export GOROOT=/usr/local/go/
export GOPATH=/mnt/c/Dropbox/Andrew/code/go/
export GO111MODULE=on
export PATH=${PATH}:${GOPATH}bin:${GOROOT}bin

gmodon() {
    export GO111MODULE=on;
}

gmodoff() {
    unset GO111MODULE;
}

gmodon
