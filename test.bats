#!/usr/bin/env bats

setup () {
  morley_script="./scripts/morley.sh"
  bin_dir="tmp"
  custom_executable="--morley_executable $bin_dir/morley"
  contract="contracts/add1.tz"
  db="bats_db.json"
  morley="$morley_script $custom_executable"
  genesisAddress="tz1f1S7V2hZJ3mhj47djb5j1saek8c2yB2Cx"
}

@test "invoking ./morley.sh without arguments" {
  $morley
}

@test "invoking ./morley.sh --help" {
  $morley --help
}

@test "invoking ./morley.sh --version" {
  $morley --version
}

@test "invoking ./morley.sh parse with correct arguments" {
  $morley parse --contract "$contract" --expand-macros
}

@test "invoking ./morley.sh typecheck with correct arguments" {
  $morley typecheck --contract "$contract" --verbose
}

@test "invoking ./morley.sh run with correct arguments" {
  $morley run --contract "$contract" \
    --db "$db" --storage 1 --parameter 1 --amount 1 --verbose \
    --now 0 --max-steps 1000 --balance 100 --write
  rm "$db"
}

@test "invoking ./morley.sh originate with correct arguments" {
  $morley originate --contract "$contract" \
    --db "$db" \
    --manager $genesisAddress \
    --delegate $genesisAddress \
    --spendable --delegatable --storage 1 --balance 1 --verbose
  rm "$db"
}

@test "invoking ./morley.sh transfer with correct arguments" {
  $morley transfer --db "$db" \
    --to $genesisAddress \
    --sender $genesisAddress \
    --parameter 1 --amount 0 --now 0 --max-steps 1000 \
    --verbose --dry-run
}

@test "invoking ./morley.sh print with correct arguments" {
  $morley print --contract "$contract"
}

@test "run contract with big_map passed in storage" {
  $morley run --contract \
    contracts/big_map_in_storage.tz  --storage 'Pair {} 0' --parameter 1
  $morley run --contract \
    contracts/big_map_in_storage.tz  --storage 'Pair {Elt 3 5; Elt 4 6} 0' --parameter 1
  run $morley run --contract \
    contracts/big_map_in_storage.tz  --storage 'Pair {Elt 4 5; Elt 3 6} 0' --parameter 1
  [ "$status" -ne 0 ]
}

@test "invoking morley to parse contract with cyrillic comments from stdin" {
  cat contracts/add1_with_cyrillic_comments.tz | $morley parse
}

@test "invoking morley to print contract with cyrillic comments" {
  $morley print --contract contracts/add1_with_cyrillic_comments.tz
}

@test "invoking morley to parse an invalid contract with non-ascii characters in error message" {
  $morley parse --contract contracts/unparsable/non-ascii-error.mtz 2>&1 | grep 'unknown type'
}

@test "lorentz registry finds some contracts" {
  $bin_dir/lorentz-contracts -- print -n walker
}

@test "invoking ./morley.sh print -o" {
  $morley print --contract contracts/first.tz -o output.mtz
  rm output.mtz
}