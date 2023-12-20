#Memscrub

## Introduction
This is a cache-aware memory scrubber. Memory scrubbers walk through all
memory protected by EDAC hardware. Any location that has been corrupted,
say be a stray radiation particle, will trigger a fault. Whether the
fault is handled by hardware of software the EDAC code will be used to
determine the correct value, which is then written back to the original
location. This is not visible to Memscrub, Memscrub just triggers the
fault.

If there are too many errors for the EDAC to correct, the fault will be
handled in software, which may power cycle the system to clear fault
conditions.

Using a simple approach to scrubbing by walking through the RAM addresses
sequential will cause all cached data to be invalided after only a small
number of memory access. When the memory scrubber returns, the cache will
need to be reloaded with process-specific data. Memscrub takes a different
approach. It walks through all of the memory associated with a given
cache line, then all of the memory associated with the next cache line,
etc. 

Completely scrubbing all RAM in a system can take a while, so memory is
generally scrubbed a piece at a time. This can be done explicitly, by telling
Memscrub to only scrub some number of locations at a time, or implicitly
by preempting Memscrub. In either case, many cachelines will still
contain information from the previous thread, which won't incur the
performance hit of reloading all cache data it was using.

See the source for more details on memory scrubbing.

##Use
Memscrub needs access to the RAM to be scrubbed. This will normally be
all EDAC-protected RAM in the system, but Memscrub provides enough
flexibility that, if it was useful, it could be run within the thread of
a single process. In the case where it is scanning all RAM in a kernel
that provides separate virtual memory addresses for processes, there are
two approaches:

| Approach | Description | Pros |
| --- | --- | --- |
| Kernel thread | Memscrub is part of the |* Has direct access to |
| | kernel |  RAM |
| | |
| | | *Kernel can present |
| | |  scrubbing interfaces in |
| | |  a configuration |
| | |  filesystem |
| |
| Privileged process | Memscrub is a process with |* Supporting userspace
| | privledges allowing mapping |access to kernel memory |
| | all RAM to a virual address |may present a security |
| |risk |

In both cases, Memscrub may need to change memory mappings in order to
be able to read the whole memory. The operations to do this are
system-specific but can be integrated with Memscrub in a straight forward
fashion,
