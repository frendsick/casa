set pagination off
set confirm off
break fn___casa_bytecode__precompute_control_flow_targets
commands
  silent
  python
import gdb

try:
    probe_hit += 1
except NameError:
    probe_hit = 1

if probe_hit in (1, 2, 11, 101):
    inferior = gdb.selected_inferior()

    def word(address):
        return int.from_bytes(inferior.read_memory(address, 8).tobytes(), "little")

    high_water = int(gdb.parse_and_eval("*(unsigned long *)&heap_ptr"))
    reusable = 0
    blocks = 0

    large = int(gdb.parse_and_eval("*(unsigned long *)&free_list"))
    while large:
        reusable += word(large) + 8
        blocks += 1
        large = word(large + 8)

    small_base = int(gdb.parse_and_eval("&small_free_lists"))
    for offset in range(0, 512, 8):
        current = word(small_base + offset)
        while current:
            reusable += word(current) + 8
            blocks += 1
            current = word(current + 8)

    rss = "unknown"
    with open(f"/proc/{inferior.pid}/status", encoding="utf-8") as status:
        for line in status:
            if line.startswith("VmRSS:"):
                rss = line.split(":", 1)[1].strip()
                break

    print(
        f"completed={probe_hit - 1} heap_high_water={high_water} "
        f"reusable={reusable} live_or_padding={high_water - reusable} "
        f"free_blocks={blocks} rss={rss}"
    )
  end
  continue
end
run
