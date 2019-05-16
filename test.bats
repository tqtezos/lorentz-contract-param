#!/usr/bin/env bats

setup () {
  morley_script="./scripts/morley.sh"
  custom_executable="--morley_executable tmp/morley"
  contract="contracts/add1.tz"
  db="bats_db.json"
  morley="$morley_script $custom_executable"
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
    --manager tz1Yz3VPaCNB5FjhdEVnSoN8Xv3ZM8g2LYhw \
    --delegate tz1Yz3VPaCNB5FjhdEVnSoN8Xv3ZM8g2LYhw \
    --spendable --delegatable --storage 1 --balance 1 --verbose
  rm "$db"
}

@test "invoking ./morley.sh transfer with correct arguments" {
  $morley transfer --db "$db" \
    --to tz1Yz3VPaCNB5FjhdEVnSoN8Xv3ZM8g2LYhw \
    --sender tz1Yz3VPaCNB5FjhdEVnSoN8Xv3ZM8g2LYhw \
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