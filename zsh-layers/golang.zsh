###############################################################################
# Go
#
export GOPATH=${HOME}/go/
export GO111MODULE=on
export PATH=${PATH}:${GOPATH}bin:${GOROOT}bin

gmodon() {
    export GO111MODULE=on;
}

gmodoff() {
    unset GO111MODULE;
}

gmodon
