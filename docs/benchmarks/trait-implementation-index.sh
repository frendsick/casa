#!/usr/bin/env bash
set -euo pipefail

compiler=${1:?usage: trait-implementation-index.sh COMPILER [COUNT...]}
shift
if [ "$#" -eq 0 ]; then
    set -- 0 250 500 1000 2000 4000
fi
runs=${BENCHMARK_RUNS:-3}
benchmark_dir=$(mktemp -d /tmp/casa-trait-index.XXXXXX)
trap 'rm -rf "$benchmark_dir"' EXIT

printf 'implementations median_s runs_s\n'
for count in "$@"; do
    source_file="$benchmark_dir/traits-$count.casa"
    output_file="$benchmark_dir/traits-$count"
    times_file="$benchmark_dir/traits-$count.times"
    {
        printf 'trait Target { fn read self:self -> i64 }\n'
        printf 'struct Subject { value: i64 }\n'
        printf 'impl Subject: Target { fn read self:Subject -> i64 { self.value } }\n'
        index=0
        while [ "$index" -lt "$count" ]; do
            printf 'trait Noise%s { }\n' "$index"
            printf 'struct Value%s { value: i64 }\n' "$index"
            printf 'impl Value%s: Noise%s { }\n' "$index" "$index"
            index=$((index + 1))
        done
        printf 'fn use[T: Target] value:T -> i64 { value .read }\n'
        printf '1 Subject use drop\n'
    } >"$source_file"

    "$compiler" "$source_file" -o "$output_file" >/dev/null
    : >"$times_file"
    run=0
    while [ "$run" -lt "$runs" ]; do
        /usr/bin/time -f '%e' -o "$times_file" -a \
            "$compiler" "$source_file" -o "$output_file" >/dev/null
        run=$((run + 1))
    done
    median=$(sort -n "$times_file" | awk '{ values[NR] = $1 } END { print values[int((NR + 1) / 2)] }')
    printf '%s %s %s\n' "$count" "$median" "$(paste -sd, "$times_file")"
done
