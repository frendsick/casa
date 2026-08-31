#!/usr/bin/env bash
set -euo pipefail

compiler=${1:?usage: operation-rewrites.sh COMPILER [COUNT...]}
shift
if [ "$#" -eq 0 ]; then
    set -- 500 1000 2000 4000
fi
runs=${BENCHMARK_RUNS:-3}
benchmark_dir=$(mktemp -d /tmp/casa-operation-rewrites.XXXXXX)
trap 'rm -rf "$benchmark_dir"' EXIT

printf 'rewrite_groups source_ops analysis_median_s wall_median_s analysis_runs_s wall_runs_s\n'
for count in "$@"; do
    source_file="$benchmark_dir/operations-$count.casa"
    output_file="$benchmark_dir/operations-$count"
    times_file="$benchmark_dir/operations-$count.times"
    analysis_file="$benchmark_dir/operations-$count.analysis"
    progress_file="$benchmark_dir/operations-$count.progress"
    {
        printf 'struct __casa_std__String { data: str }\n'
        printf 'impl str { fn to_str self:$str -> __casa_std__String { self copy __casa_std__String } }\n'
        printf 'trait __casa_std__Display { fn to_str self:$self -> __casa_std__String }\n'
        printf 'struct Tag { value: i64 }\n'
        printf 'impl Tag: __casa_std__Display {\n'
        printf '    fn to_str self:$Tag -> __casa_std__String { "tag".to_str }\n'
        printf '}\n'
        printf 'fn benchmark {\n'
        printf '    1 Tag = tag\n'
        index=0
        while [ "$index" -lt "$count" ]; do
            printf '    tag print\n'
            index=$((index + 1))
        done
        printf '}\n'
    } >"$source_file"

    "$compiler" "$source_file" -o "$output_file" >/dev/null
    : >"$times_file"
    : >"$analysis_file"
    run=0
    while [ "$run" -lt "$runs" ]; do
        /usr/bin/time -f '%e' -o "$times_file" -a \
            "$compiler" --verbose "$source_file" -o "$output_file" \
            > /dev/null 2>"$progress_file"
        awk '/Compiling bytecode/ { value = $2; sub(/s$/, "", value); print value }' \
            "$progress_file" >>"$analysis_file"
        run=$((run + 1))
    done
    analysis_median=$(sort -n "$analysis_file" | awk '{ values[NR] = $1 } END { print values[int((NR + 1) / 2)] }')
    wall_median=$(sort -n "$times_file" | awk '{ values[NR] = $1 } END { print values[int((NR + 1) / 2)] }')
    printf '%s %s %s %s %s %s\n' \
        "$count" "$((count * 2 + 3))" "$analysis_median" "$wall_median" \
        "$(paste -sd, "$analysis_file")" "$(paste -sd, "$times_file")"
done
