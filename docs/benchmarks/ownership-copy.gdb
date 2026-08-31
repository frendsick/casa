set pagination off
set confirm off
set $allocation_calls = 0
break heap_alloc
commands
  silent
  set $allocation_calls = $allocation_calls + 1
  continue
end
run
printf "heap_alloc_calls=%d\n", $allocation_calls
