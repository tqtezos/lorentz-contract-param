#! /usr/bin/env bash

if ! docker -v > /dev/null 2>&1 ; then
    echo "Docker does not seem to be installed."
    exit 1
fi

docker_dir="$HOME/.morley"
mnt_dir="/mnt"
mkdir -p "$docker_dir"
docker_pull_timestamp="$docker_dir/docker_pull.timestamp"
docker_image=registry.gitlab.com/tezos-standards/morley

maybe_pull_image() {
    if [ ! -f "$docker_pull_timestamp" ] \
         || [ 3600 -le $(($(date "+%s") - $(cat "$docker_pull_timestamp"))) ]; then
        pull_image
    fi
}

pull_image() {
    docker pull "$docker_image"
    date "+%s" >| "$docker_pull_timestamp"
}

maybe_pull_image

manpage() {
    docker run $docker_image morley --help
    echo ""
    echo "Also you can use --docker_debug to see additional informations such as"
    echo "arguments that are being passed to docker run"
}

if [ "$#" -eq 0 ];
then
    manpage
    exit 0
fi

typeset -a args;

subcommand="$1"
shift
args+=("$subcommand")
default_db_filepath=".db.json"
# ^ Default json database stored in ~/.morley
while true;
do
  arg="$1"
  if [[ -z "$arg" ]];
  then
      break
  fi
  case $arg in
    --contract )
        contract_filepath="$2"
        dn=$(dirname "$contract_filepath")
        mkdir -p "$docker_dir/contract/$dn/"
        cp "$contract_filepath" "$docker_dir/contract/$contract_filepath"
        args+=("$arg" "$mnt_dir/contract/$contract_filepath")
        shift 2
        ;;
    --db )
        user_db_filepath="$2"
        shift 2
        ;;
    --docker_debug )
        debug_flag=true
        shift
        ;;
    * )
        args+=("$arg")
        shift
  esac
done
if [ "$user_db_filepath" != "" ];
then
    dn=$(dirname "$user_db_filepath")
    mkdir -p "$docker_dir/db/$dn"
    touch "$user_db_filepath"
    # ^ Touch in case given user_db_filepath doesn't exist
    ln "$user_db_filepath" "$docker_dir/db/$user_db_filepath"
fi
if [ "$user_db_filepath" = "" ];
then
    user_db_filepath=$default_db_filepath
fi
if [ "$subcommand" != "parse" ] && [ "$subcommand" != "typecheck" ] && [ "$subcommand" != "print" ];
then
    args+=("--db" "$mnt_dir/db/$user_db_filepath")
fi
if [ -n "$debug_flag" ];
then
    echo "docker run arguments: ${args[*]}"
fi
docker run -v "$docker_dir:$mnt_dir" -i $docker_image morley "${args[@]}"
run_exitcode=$?
rm -rf "$docker_dir/contract"
if [ "$user_db_filepath" != "$default_db_filepath" ];
then
    rm "$docker_dir/db/$user_db_filepath"
fi
exit "$run_exitcode"
