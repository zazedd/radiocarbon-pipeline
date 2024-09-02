name=$(cat ./env/account.txt)
gappid=$(cat ./env/gappid.txt)
instid=$(cat ./env/ginstid.txt)

green="\033[1;32m"
violet="\033[3;35m"
reset="\033[0m"
green_bold="\033[1;32m"

echo "${green}Starting pipeline with: Github name -> ${violet}${name}${green_bold}; Github app id -> ${violet}${gappid}${green_bold}; Github installation id -> ${violet}${instid}${reset}"

dune exec -- radiocarbon --github-account-allowlist $name --github-app-id $gappid --github-private-key-file env/sk.pem --github-webhook-secret-file env/wsk.txt --github-app-account $name --github-inst-id $instid .
