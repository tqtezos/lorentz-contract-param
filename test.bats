#!/usr/bin/env bats

setup () {
  morley="./scripts/morley.sh"
  contract="contracts/add1.tz"
  db="bats_db.json"
  custom_executable="--morley_executable tmp/morley"
}

@test "invoking ./morley.sh without arguments" {
  run ./scripts/morley.sh $custom_executable
  [ "$status" -eq 0 ]
}

@test "invoking ./morley.sh --help" {
  run ./scripts/morley.sh $custom_executable --help
  [ "$status" -eq 0 ]
}

@test "invoking ./morley.sh --version" {
  run ./scripts/morley.sh $custom_executable --version
  [ "$status" -eq 0 ]
}

@test "invoking ./morley.sh parse with correct arguments" {
  run "$morley" $custom_executable parse --contract "$contract" \
  --expand-macros
  [ "$status" -eq 0 ]
}

@test "invoking ./morley.sh typecheck with correct arguments" {
  run "$morley" $custom_executable typecheck --contract "$contract" \
  --verbose
  [ "$status" -eq 0 ]
}

@test "invoking ./morley.sh run with correct arguments" {
  run "$morley" $custom_executable run --contract "$contract" \
  --db "$db" --storage 1 --parameter 1 --amount 1 --verbose \
  --now 0 --max-steps 1000 --balance 100 --write
  [ "$status" -eq 0 ]
  rm "$db"
}

@test "invoking ./morley.sh originate with correct arguments" {
  run "$morley" $custom_executable originate --contract "$contract" \
  --db "$db" \
  --manager tz1Yz3VPaCNB5FjhdEVnSoN8Xv3ZM8g2LYhw \
  --delegate tz1Yz3VPaCNB5FjhdEVnSoN8Xv3ZM8g2LYhw \
  --spendable --delegatable --storage 1 --balance 1 --verbose
  [ "$status" -eq 0 ]
  rm "$db"
}

@test "invoking ./morley.sh transfer with correct arguments" {
  run "$morley" $custom_executable transfer --db "$db" \
  --to tz1Yz3VPaCNB5FjhdEVnSoN8Xv3ZM8g2LYhw \
  --sender tz1Yz3VPaCNB5FjhdEVnSoN8Xv3ZM8g2LYhw \
  --parameter 1 --amount 0 --now 0 --max-steps 1000 \
  --verbose --dry-run
  [ "$status" -eq 0 ]
}

@test "invoking ./morley.sh print with correct arguments" {
  run "$morley" $custom_executable print --contract "$contract"
  [ "$status" -eq 0 ]
}
