#! /usr/bin/env bash
set -e

docker_dir="$HOME/.morley"
mnt_dir="/mnt"
mkdir -p "$docker_dir"
docker_image=registry.gitlab.com/tezos-standards/morley

maybe_pull_image() {
    if [ ! -f "$docker_pull_timestamp" ] \
           || [ 3600 -le $(($(date "+%s") - $(cat "$docker_pull_timestamp"))) ];
    then
        pull_image
    fi
}

pull_image() {
    docker pull "$docker_image"
    date "+%s" >| "$docker_pull_timestamp"
}

if [ "$1" = "--morley_executable" ];
then
    executable_filepath="$2"
    shift 2
else
    if ! docker -v > /dev/null 2>&1 ; then
        echo "Docker does not seem to be installed."
        exit 1
    fi
    typeset -a newargs;

    for arg in "$@"
    do
        if [ "$arg" = "--latest" ];
        then
            latest_flag=true
        else
            newargs+=("$arg")
        fi
    done

    # Drop --latest from arguments if it is presented
    set -- "${newargs[@]}"

    if [ -n "$latest_flag" ];
    then
        docker_image="$docker_image:latest"
        docker_pull_timestamp="$docker_dir/docker_pull_latest.timestamp"
    else
        docker_image="$docker_image:production"
        docker_pull_timestamp="$docker_dir/docker_pull_production.timestamp"
    fi

    maybe_pull_image
fi

manpage() {
    if [ "$executable_filepath" = "" ]
    then
        docker run $docker_image morley --help
    else
        $executable_filepath --help
    fi
    echo ""
    echo "You can pass --morley_executable fith filepath as a first argument to provide"
    echo "custom morley executable (it's mostly used for testing this shell script)"
    echo ""
    echo "Also you can use --docker_debug to see additional informations such as"
    echo "arguments that are being passed to docker run"
    echo ""
    echo "You can pass --latest to use docker image that is built from latest version"
    echo "of master branch. Otherwise latest version from production branch"
    echo "will be used"
}

if [[ "$#" -eq 0 ]] || [[ "$#" -eq 1 && "$1" = "--latest" ]];
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
        if [ "$executable_filepath" = "" ]
        then
            dn=$(dirname "$contract_filepath")
            mkdir -p "$docker_dir/contract/$dn/"
            cp "$contract_filepath" "$docker_dir/contract/$contract_filepath"
            args+=("$arg" "$mnt_dir/contract/$contract_filepath")
        else
            args+=("$arg" "$contract_filepath")
        fi

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
if [ "$user_db_filepath" != "" ] && [ "$executable_filepath" = "" ];
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
    if [ "$executable_filepath" = "" ]
    then
        args+=("--db" "$mnt_dir/db/$user_db_filepath")
    else
        args+=("--db" "$user_db_filepath")
    fi

fi
if [ -n "$debug_flag" ];
then
    echo "docker run arguments: ${args[*]}"
fi

set +e
if [ "$executable_filepath" = "" ];
then
    docker run -v "$docker_dir:$mnt_dir" -i $docker_image morley "${args[@]}"
else
    $executable_filepath "${args[@]}"
fi

run_exitcode=$?
set -e

rm -rf "$docker_dir/contract"
if [ "$user_db_filepath" != "$default_db_filepath" ] && [ "$executable_filepath" = "" ];
then
    rm -rf "$docker_dir/db/"
fi
exit "$run_exitcode"
