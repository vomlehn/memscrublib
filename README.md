This is a cache-aware memory scrubber. That is, it reads all
locations that use a particular cache index, then all locations
with the next cache index, etc. Since memory scrubbing must
hit all cache lines, using this to scrub memory a few cache
lines at a time evens out the memory scrubbing performance hit.

See the source for more details on memory scrubbing.

Note also that I am looking forward to more things, like code
do the processor-specific things to inject ECC failures. Due
to this, the name may change. Right now, I'm thinking of
cachekit.
