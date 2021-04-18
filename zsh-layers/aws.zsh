export AWS_DEFAULT_REGION=us-east-2
export AWS_REGION=us-east-2

gac() {
  env="$1"
  $(get-aws-credentials $env)
}

