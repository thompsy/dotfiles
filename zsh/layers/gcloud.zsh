GCLOUD_ROOT=~/.google-cloud-sdk/

if [ -d ${GCLOUD_ROOT} ]; then
    source "${GCLOUD_ROOT}/path.zsh.inc"
    source "${GCLOUD_ROOT}/completion.zsh.inc"
fi
