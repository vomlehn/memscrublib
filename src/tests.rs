#[cfg(test)]
mod tests {
    use num_traits::Num;

    //    use std::borrow::BorrowMut;
    use std::fmt;
    use std::marker::PhantomData;
    use std::ops::Index;
    //    use std::ptr;
    use std::slice;
    use std::time::Instant;

    use crate::*;

    // Type used for the read counter
    type NRead = u8;

    // Traits for testing
    // ==================
    trait TestCachelineDataBase<D, const S: usize>:
        CachelineDataBase<D, S> + Index<usize>
    where
        D: Num,
    {
    }

    trait TestCachelineBase<D, const S: usize>:
        TestCachelineBase_<D, S>
    where
        D: Num,
    {
    }

    trait TestCachelineBase_<D, const S: usize>: CachelineBase<D, S>
    where
        D: Num,
    {
    }

    // TestCacheBase - Description of the cache for basic tests
    // cache_index_width - Number of times this cacheline was iit during the
    //      scrub
    // read_infos:          Array of ReadInfo items>
    trait TestCacheBase<
        'a,
        CL,
        CLD,
        const N: usize,
        const W: usize,
        D,
        const S: usize,
    >: CacheBase<CL, CLD, N, W, D, S>
    where
        CL: CachelineBase<D, S>, /* + ?Sized */
        CLD: CachelineDataBase<D, S> + ?Sized,
        D: Num + 'a,
    {
        fn new() -> Self
        where
            Self: Sized;
        fn cacheline(&self) -> *const TestCacheline<D, S>;
    }

    trait TestMemoryScrubberBase<
        'a,
        C,
        CL,
        CLD,
        const N: usize,
        const W: usize,
        D,
        const S: usize,
        M,
    >: MemoryScrubberBase<C, CL, CLD, N, W, D, S, M>
    where
        C: TestCacheBase<'a, CL, CLD, N, W, D, S>,
        // Not sure why I have to add CachelineBase here
        CL: TestCachelineBase<D, S> + CachelineBase<D, S>,
        CLD: TestCachelineDataBase<D, S>,
        D: Num + 'a,
        M: MemAreaBase<VAddr>,
    {
        /*
                fn check_scrubber_params(cache: &C, scrub_areas: &[MemArea]) ->
                    Result<(), Error> where Self: Sized {
            println!("In TestMemoryScrubberBase");
                    <TestMemoryScrubber<C, CL, CLD, N, W, D, S, M> as
                            MemoryScrubberBase<C, CL, CLD, N, W, D, S, M>>
                        ::check_scrubber_params(cache, scrub_areas)?;
            println!("In TestMemoryScrubberBase: no problem");
                        Ok(())
                }
        */
    }

    // Types for testing
    // =================
    struct TestCache<
        'a,
        CL,
        CLD,
        const N: usize,
        const W: usize,
        D,
        const S: usize,
    >
    where
        CLD: TestCachelineDataBase<D, S>,
        D: Num,
    {
        cacheline: TestCacheline<D, S>,
        // FIXME: Remove when possible. Right now, the compiler doesn't appear
        // to know that U is actually used when it's in CacheBase<CL>. So, this
        // works around that problem
        _marker1: PhantomData<CL>,
        _marker2: PhantomData<CLD>,
        _marker3: PhantomData<D>,
    }

    // /*
    impl<
            'a,
            CL,
            CLD,
            const N: usize,
            const W: usize,
            D,
            const S: usize,
        > TestCache<'_, CL, CLD, N, W, D, S>
    where
        CL: TestCachelineBase<D, S>,
        CLD: TestCachelineDataBase<D, S>,
        D: Num,
    {
        fn new(sizes: &[usize]) -> Self {
            let cacheline = TestCacheline::new();

            let test_cache = Self {
                cacheline: cacheline,
                _marker1: PhantomData,
                _marker2: PhantomData,
                _marker3: PhantomData,
            };

            test_cache
        }
    }

    /*
        impl<'a, CL, CLD, const N: usize, const W: usize, D, const S: usize>
        Test<'a, CLD, N, W, D, S>
        for TestCache<CL, CLD, N, W, D, S>
        where
            CL: TestCachelineBase<D, S>,
            CLD: TestCachelineDataBase<D, S>,
            D: Num,
        {
            fn test(&mut self, _cacheline_ptr: *const CLD) {
    panic!("TestCache::Test unimplemented");
            }
        }
    */

    impl<
            'a,
            CL,
            CLD,
            const N: usize,
            const W: usize,
            D,
            const S: usize,
        > CacheBase<CL, CLD, N, W, D, S> for TestCache<'_, CL, CLD, N, W, D, S>
    where
        CL: TestCachelineBase<D, S> + CachelineBase<D, S>,
        CLD: TestCachelineDataBase<D, S>,
        D: Num,
    {
    }

    impl<
            'a,
            CL,
            CLD,
            const N: usize,
            const W: usize,
            D,
            const S: usize,
        > TestCacheBase<'a, CL, CLD, N, W, D, S>
        for TestCache<'_, CL, CLD, N, W, D, S>
    where
        CL: TestCachelineBase<D, S>,
        CLD: TestCachelineDataBase<D, S>,
        D: Num + 'a,
    {
        fn new() -> Self {
            let cacheline = TestCacheline::new();

            let test_cache = Self {
                cacheline: cacheline,
                _marker1: PhantomData,
                _marker2: PhantomData,
                _marker3: PhantomData,
            };

            test_cache
        }
        fn cacheline(&self) -> *const TestCacheline<D, S> {
            &self.cacheline
        }
    }

    impl<
            'a,
            CL,
            CLD,
            const N: usize,
            const W: usize,
            D,
            const S: usize,
        > fmt::Debug for TestCache<'_, CL, CLD, N, W, D, S>
    where
        CL: TestCachelineBase<D, S>,
        CLD: TestCachelineDataBase<D, S>,
        D: Num,
    {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{:?}", self.cacheline)
        }
    }

    // This clues the compiler in that I know what I'm doing by having a
    // *const pointer in the struct
    unsafe impl<
            'a,
            CL,
            CLD,
            const N: usize,
            const W: usize,
            D,
            const S: usize,
        > Sync for TestCache<'_, CL, CLD, N, W, D, S>
    where
        CL: TestCachelineBase<D, S>,
        CLD: TestCachelineDataBase<D, S>, /* + ?Sized */
        D: Num,
    {
    }

    impl<
            'a,
            CL,
            CLD,
            const N: usize,
            const W: usize,
            D,
            const S: usize,
        > Clone for TestCache<'_, CL, CLD, N, W, D, S>
    where
        CL: TestCachelineBase<D, S>,
        CLD: TestCachelineDataBase<D, S>,
        D: Num,
    {
        fn clone(&self) -> Self {
            TestCache {
                cacheline: self.cacheline.clone(),
                _marker1: PhantomData,
                _marker2: PhantomData,
                _marker3: PhantomData,
            }
        }
    }
    // */
    #[derive(Clone, Copy)]
    #[repr(C)]
    struct TestCachelineData<D, const S: usize>
    where
        D: Num,
    {
        data: [D; S],
    }

    impl<D, const S: usize> CachelineDataBase<D, S> for TestCachelineData<D, S> where
        D: Num
    {
    }

    // FIXME: this seems to be where the problem starts
    impl<D, const S: usize> TestCachelineDataBase<D, S>
        for TestCachelineData<D, S>
    where
        D: Num,
    {
    }

    impl<D, const S: usize> Index<usize> for TestCachelineData<D, S>
    where
        D: Num,
    {
        type Output = D;

        fn index(&self, i: usize) -> &Self::Output {
            &self.data[i]
        }
    }

    struct TestCacheline<D, const S: usize>
    where
        D: Num,
    {
        _marker1: PhantomData<D>,
    }

    impl<D, const S: usize> TestCacheline<D, S>
    where
        D: Num,
    {
        fn new() -> Self {
            TestCacheline {
                _marker1: PhantomData,
            }
        }
    }

    impl<D, const S: usize> CachelineBase<D, S> for TestCacheline<D, S> where
        D: Num
    {
    }

    impl<D, const S: usize> TestCachelineBase<D, S> for TestCacheline<D, S> where
        D: Num
    {
    }

    impl<D, const S: usize> TestCachelineBase_<D, S> for TestCacheline<D, S>
    where
        D: Num,
    {
        /*
                fn new() -> Self where Self: Sized {
                    TestCacheline {
                    }
                }
        */
    }

    impl<D, const S: usize> Clone for TestCacheline<D, S>
    where
        D: Num,
    {
        fn clone(&self) -> TestCacheline<D, S> {
            TestCacheline {
                _marker1: PhantomData,
            }
        }
    }

    impl<D, const S: usize> fmt::Debug for TestCacheline<D, S>
    where
        D: Num,
    {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{:?}", "TestCacheline...")
        }
    }

    struct TestMemoryScrubber<
        'a,
        C,
        CL,
        CLD,
        const N: usize,
        const W: usize,
        D,
        const S: usize,
        M,
    >
    where
        C: TestCacheBase<'a, CL, CLD, N, W, D, S>,
        CLD: TestCachelineDataBase<D, S>,
        CL: CachelineBase<D, S>,
        D: Num + 'a,
        M: MemAreaBase<VAddr>,
    {
        cache: C,
        my_scrub_areas: &'a [M],
        _marker1: PhantomData<C>,
        _marker2: PhantomData<CL>,
        _marker3: PhantomData<CLD>,
        _marker4: PhantomData<D>,
    }

    impl<
            'a,
            C,
            CL,
            CLD,
            const N: usize,
            const W: usize,
            D,
            const S: usize,
            M,
        > TestMemoryScrubber<'a, C, CL, CLD, N, W, D, S, M>
    where
        C: TestCacheBase<'a, CL, CLD, N, W, D, S>,
        CL: TestCachelineBase<D, S> + CachelineBase<D, S>,
        CLD: TestCachelineDataBase<D, S>,
        D: Num + 'a,
        M: MemAreaBase<VAddr>,
    {
        fn new(
            scrub_areas: &'a [M]
        ) -> Result<TestMemoryScrubber<'a, C, CL, CLD, N, W, D, S, M>, Error>
        {

            let test_cache = C::new();

            TestMemoryScrubber::<C, CL, CLD, N, W, D, S, M>::check_scrubber_params(
                &test_cache,
                scrub_areas,
            )?;

            Ok(TestMemoryScrubber::<C, CL, CLD, N, W, D, S, M> {
                cache: test_cache,
                my_scrub_areas: scrub_areas,
                _marker1: PhantomData,
                _marker2: PhantomData,
                _marker3: PhantomData,
                _marker4: PhantomData,
            })
        }
    }

    impl<
            'a,
            C,
            CL,
            CLD,
            const N: usize,
            const W: usize,
            D,
            const S: usize,
            M,
        > TestMemoryScrubberBase<'a, C, CL, CLD, N, W, D, S, M>
        for TestMemoryScrubber<'a, C, CL, CLD, N, W, D, S, M>
    where
        C: TestCacheBase<'a, CL, CLD, N, W, D, S>,
        // Not sure why I have to add CachelineBase and TestCachelineBase_ here
        CL: TestCachelineBase<D, S>
            + CachelineBase<D, S>
            + TestCachelineBase_<D, S>,
        CLD: TestCachelineDataBase<D, S>,
        D: Num + 'a,
        M: MemAreaBase<VAddr>,
    {
    }

    impl<
            'a,
            C,
            CL,
            CLD,
            const N: usize,
            const W: usize,
            D,
            const S: usize,
            M,
        > MemoryScrubberBase<C, CL, CLD, N, W, D, S, M>
        for TestMemoryScrubber<'a, C, CL, CLD, N, W, D, S, M>
    where
        C: TestCacheBase<'a, CL, CLD, N, W, D, S>,
        // Not sure why I have to add CachelineBase here
        CL: TestCachelineBase<D, S> + CachelineBase<D, S>,
        CLD: TestCachelineDataBase<D, S>,
        D: Num + 'a,
        M: MemAreaBase<VAddr>,
    {
        fn cache(&self) -> &C {
            println!("TestMemoryScrubber:MemoryScrubberBase1: entered");
            &self.cache as &C
        }

        fn scrub_areas(&self) -> &[M] {
            &self.my_scrub_areas
        }
    }

    impl<
            'a,
            C,
            CL,
            CLD,
            const N: usize,
            const W: usize,
            D,
            const S: usize,
            M,
        > VerifyScrubber
        for TestMemoryScrubber<'a, C, CL, CLD, N, W, D, S, M>
    where
        C: TestCacheBase<'a, CL, CLD, N, W, D, S>,
        // Not sure why I have to add CachelineBase here
        CL: TestCachelineBase<D, S> + CachelineBase<D, S>,
        CLD: TestCachelineDataBase<D, S>,
        D: Num + 'a,
        M: MemAreaBase<VAddr>,
    {
        fn verify(&mut self) {
            // FIXME: what does this code actually do?
//let x: u8 = self.cache;
            let cacheline_ptr: * const TestCacheline<D, S> = self.cache.cacheline();

            let test_cacheline_data = TestCachelineData::<OkD, OK_S>{
                data: [0; OK_S]
            };
            let mut data: [TestCachelineData<OkD, OK_S>; 10] =
                [test_cacheline_data; 10];
            let cacheline_ptr: * const TestCachelineData<OkD, OK_S> =
                data.as_ptr();
            let read_infos = &self.read_infos;

            // Ensure that allocated_area is used
            for read_info in read_infos {
                let allocated_area = &read_info.mem.allocated_area;
// FIXME: what is this next statement supposed to be?
                assert_eq!(allocated_area[0], allocated_area[0]);
            }
        }
    }

    trait VerifyScrubber {
        fn verify(&mut self);
    }

        struct Tester<'a, CL, CLD, const N: usize, const W: usize, D, const S: usize>
        where
            // Not sure why I have to add CachelineBase and TestCachelineBase_ here
            CL: TestCachelineBase<D, S> + CachelineBase<D, S> + TestCachelineBase_<D, S>,
            CLD: TestCachelineDataBase<D, S>,
            D: Num,
        {
            cache: &'a dyn TestCacheBase<'a, CL, CLD, N, W, D, S>,
            // FIXME: can this be a reference to a slice?
            read_infos: Vec<ReadInfo>,
            _marker1: PhantomData<CL>,
        }

        impl<'a, CL, CLD, const N: usize, const W: usize, D, const S: usize> Tester<'a, CL, CLD, N, W, D, S>
        where
            CL: TestCachelineBase<D, S> + CachelineBase<D, S> + TestCachelineBase_<D, S>,
            CLD: TestCachelineDataBase<D, S>,
            D: Num,
        {
            pub fn new(
                cache: &'a TestCache<CL, CLD, N, W, D, S>,
                sizes: &'a [usize],
            ) -> Tester<'a, CL, CLD, N, W, D, S> {
                let mut read_infos = Vec::<ReadInfo>::new();

                // Allocate memory and associated data structures
                for size in sizes {
                    let mem = Mem::new::<CL, D, S>(*size as VAddr);

                    let scrub_area_size = CL::size_in_cachelines(&mem.scrub_area);
                    let n_reads_size = GUARD_LINES + scrub_area_size + GUARD_LINES;
                    read_infos.push(ReadInfo::new(n_reads_size, mem));
                }
                Tester {
                    cache: cache,
                    read_infos: read_infos,
                    _marker1: PhantomData,
                }
            }

            pub fn test(&mut self, _cacheline_ptr: *const CL) {
                self.set_cookie(_cacheline_ptr);
                self.increment_n_reads(_cacheline_ptr);
            }

            fn set_cookie(&mut self, _cacheline_ptr: *const CL) {
                unimplemented!();
            }

            fn increment_n_reads(&mut self, _cacheline_ptr: *const CL) {
                // Update the read count
                let index = {
                    //                self.read_index(self.cache, _cacheline_ptr)
                    self.read_index(_cacheline_ptr)
                };
                let n_reads = { self.get_n_reads(_cacheline_ptr) };

                n_reads[GUARD_LINES + index] += 1;
                println!("n_reads[{}] became {}", index, n_reads[GUARD_LINES + index]);
            }

            // Compute the index into the n_read array for this address. This
            // array has GUARD_LINES elements surrounding the actual counts.
            // _cacheline_ptr: Pointer to the address
            fn read_index(
                &mut self,
                //            cache: &'a dyn TestCacheBase<CL, CLD, N, W, D, S>,
                _cacheline_ptr: *const CL,
            ) -> usize {
                let cacheline_addr = _cacheline_ptr as VAddr;
                let cacheline_size = { CL::cacheline_size() };

                let read_info = self.find_read_info(_cacheline_ptr);
                let scrub_area = read_info.mem.scrub_area.clone();
                let n_n_reads = CL::size_in_cachelines(&scrub_area);
                let start_addr = scrub_area.start() as VAddr;

                let index = (cacheline_addr - start_addr) / cacheline_size;
                assert!(index < n_n_reads);
                index
            }

            // Returns a reference to n_reads[], the array of count read counts
            fn get_n_reads(&mut self, _cacheline_ptr: *const CL) -> &mut Vec<NRead> {
                let read_info = self.find_read_info(_cacheline_ptr);
                read_info.n_reads.as_mut().unwrap()
            }

            // Find a ReadInfo corresponding to a given location in memory
            fn find_read_info(&mut self, _cacheline_ptr: *const CL) -> &mut ReadInfo {
                let cacheline_addr = _cacheline_ptr as VAddr;

                for search_read_info in &mut self.read_infos.iter_mut() {
                    let scrub_area = &search_read_info.mem.scrub_area;
                    let start_addr = scrub_area.start() as VAddr;
                    let end_addr = scrub_area.end() as VAddr;

                    if cacheline_addr >= start_addr && cacheline_addr <= end_addr {
                        return search_read_info;
                    }
                }

                // If we failed, it's because the cache addess wasn't in any of
                // the MemAreas.
                panic!("Unable to find address {:x}", cacheline_addr);
            }
        }

        impl<'a, CL, CLD, const N: usize, const W: usize, D, const S: usize> Clone
            for Tester<'a, CL, CLD, N, W, D, S>
        where
            CL: TestCachelineBase<D, S> + CachelineBase<D, S> + TestCachelineBase_<D, S>,
            CLD: TestCachelineDataBase<D, S>,
            D: Num,
        {
            fn clone(&self) -> Tester<'a, CL, CLD, N, W, D, S> {
                let read_infos = self.read_infos.clone();
                Tester {
                    cache: self.cache,
                    read_infos: read_infos,
                    _marker1: PhantomData,
                }
            }
        }

        impl<'a, CL, CLD, const N: usize, const W: usize, D, const S: usize> fmt::Debug
            for Tester<'a, CL, CLD, N, W, D, S>
        where
            CL: TestCachelineBase<D, S> + CachelineBase<D, S> + TestCachelineBase_<D, S>,
            CLD: TestCachelineDataBase<D, S>,
            D: Num,
        {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "{:?}", self)
            }
        }
    // Cache characteristics
    // BASIC_CACHELINE_WIDTH - number of bits required to index a byte in a
    //      cache line
    // BASIC_CACHE_INDEX_WIDTH - number of bits used as a cache line index in
    //      the cache
    // BASIC_MEM_SIZE - Cache size, in bytes
    const BASIC_ECCDATA_WIDTH: usize = usize::BITS as usize
        - 1
        - std::mem::size_of::<BasicECCData>().leading_zeros() as usize;
    const BASIC_CACHELINE_WIDTH: usize = 6 + BASIC_ECCDATA_WIDTH;
    const BASIC_CACHE_INDEX_WIDTH: usize = 10;
    const BASIC_MEM_SIZE: VAddr =
        1 << (BASIC_CACHELINE_WIDTH + BASIC_CACHE_INDEX_WIDTH);

    // ECCData - The data size used to compute the ECC for basic tests
    // BasicCacheline - the data type of a cache line
    type BasicECCData = u64;

    // FIXME: can these be combined?

    unsafe impl Sync for MemArea<VAddr> {}

    // FIXME: should this be moved?
    unsafe impl<
            'a,
            C,
            CL,
            CLD,
            const N: usize,
            const W: usize,
            D,
            const S: usize,
            M,
        > Sync for MemAreaIterator<'a, C, CL, CLD, N, W, D, S, M>
    where
        C: CacheBase<CL, CLD, N, W, D, S>,
        CL: CachelineBase<D, S>,
        CLD: CachelineDataBase<D, S>,
        D: Num,
        M: MemAreaBase<VAddr>,
    {
    }

    // Description of memory that is read into by the read_cacheline()
    // function. This keeps the actual allocation together with the pointer
    // into that allocation so that things go out of scope at the same time.
    //
    // allocated_area - Vec<u8> of elements that can be read by
    //      read_cacheline()
    // start - Cache size-aligned pointer of the first byte to use in allocated_area
    // end - Pointer to the last byte
    #[derive(Clone, Debug)]
    struct Mem {
        allocated_area: Vec<u8>,
        scrub_area: MemArea<VAddr>,
    }

    impl Mem {
        // Allocates a memory area on a cache line boundary
        //
        //
        // Returns: a Mem with a Vec<u8>. The size of the Vec<u8> is
        // opaque but the p element in the Mem has at least size bytes
        // starting at a cache line aligned section of memory. The size
        // element is the size used to call this function.
        //        fn new<CL, CLD, D, const S: usize>(size: VAddr) -> Mem
        fn new<CL, D, const S: usize>(size: VAddr) -> Mem
        where
            CL: CachelineBase<D, S>,
            //            CLD:    CachelineDataBase<D, S>,
            D: Num,
        {
            let cacheline_size = CL::cacheline_size();
            Self::new_aligned(size, cacheline_size)
        }

        fn new_aligned(size: VAddr, alignment_size: usize) -> Mem {
            if size == 0 {
                panic!("Allocation failure: {:?}", Error::ZeroSize);
            }

            let _dummy = value_to_width(alignment_size).expect(
                "new_alignment: alignment_size must be power of two",
            );

            // Allocate memory, which includes a cache size-sided area before
            // what we are touching. These areas should not be touched by
            // scrubbing.
            let allocated_size = alignment_size + size as usize;
            let allocated_area: Vec<u8> = vec![0; allocated_size];
            let alignment_size_addr = alignment_size as VAddr;

            // Now find the first cache line aligned pointer
            let start_addr = (allocated_area.as_ptr() as VAddr
                + alignment_size_addr
                - 1)
                & !(alignment_size_addr - 1);
            let end_addr = start_addr + size - 1;

            Mem {
                allocated_area: allocated_area,
                scrub_area: MemArea::<VAddr> {
                    s: start_addr,
                    e: end_addr,
                },
            }
        }
    }

    // This clues the compiler in that I know what I'm doing by having a
    // *const pointer in the struct
    unsafe impl Sync for Mem {}

    // Data structure a memory allocation along with counters for how many
    // times cache line-sized memory areas has been read.
    // mem:     Boundaries of the address covered by this structure
    // n_reads: Counters, one per cache line-sized area.
    #[derive(Clone, Debug)]
    struct ReadInfo {
        mem: Mem,
        n_reads: Option<Vec<NRead>>,
    }

    impl ReadInfo {
        // Allocate a vector for counters
        // size:    Size in cache lines
        // mem:     Associated memory
        fn new(size: usize, mem: Mem) -> ReadInfo {
            let n_reads = vec![0; size];
            ReadInfo {
                mem: mem,
                n_reads: Some(n_reads),
            }
        }
    }

    // Cache characteristics
    // TEST_CACHE_INDEX_WIDTH - number of bits used as a cache line index
    //  in the cache
    // TEST_CACHE_LINES - number of cache lines
    /* FIXME: restore this
        const TEST_CACHE_INDEX_WIDTH: usize = 10;
    */
    const TEST_CACHE_INDEX_WIDTH: usize = 2;
    const TEST_CACHE_LINES: usize = 1 << TEST_CACHE_INDEX_WIDTH;

    // GUARD_LINES - Number of cache line size items we allocate but don't
    //      touch, to verify we don't touch the wrong place.
    // TEST_CACHE_NUM_TOUCHED - Number of cache footprints we use for
    //      testing
    // TEST_SANDBOX_SIZE - Number of cachelines we actually expect to
    //      touch
    // TEST_COOKIE - Cookie written to all memory we're scrubbing
    const GUARD_LINES: usize = TEST_CACHE_LINES;
    const TEST_CACHE_NUM_TOUCHED: usize = 3;
    const TEST_SANDBOX_SIZE: usize =
        TEST_CACHE_LINES * TEST_CACHE_NUM_TOUCHED;
    const TEST_COOKIE: usize = 17;

    // Verify an error is returned if the number of cache lines, the size of
    // the basic data, or the number of cache line items is not a power of two
    // OK_N:     Number of cache lines
    // OK_W:     Number of ways in each cache line
    // OkD:      Type of the smallest unit written to memory
    // OK_S:     Number of OkD items in a cache line
    const OK_N: usize = 1024;
    const OK_W: usize = 16;
    type OkD = u32;
    const OK_S: usize = 8;

/*
    // Verify that the number of cache lines is a power of two
    #[test]
    fn test_unaligned_parameters() {
        test_unaligned_parameter(OK_N, "N - number of cache lines");
        test_unaligned_parameter(OK_W, "N - number of cache ways");
        test_unaligned_parameter(
            std::mem::size_of::<OkD>(),
            "size_of<D> - size of cache line element",
        );
        test_unaligned_parameter(
            OK_S,
            "S - number of cache line elements per cache line",
        );
    }

    fn test_unaligned_parameter(value: usize, label: &str) {
        let width = value_to_width::<usize>(value);
        if width.is_err() {
            panic!("Value must be a power of two: {}", label);
        }
    }

    fn test_scrubber_deleteme<
        'a,
        const N: usize,
        const W: usize,
        D,
        const S: usize,
    >(
        param_name: &str,
    ) where
        D: Num,
    {
        // Want to allocate a few times the cache size for this
        const CACHE_SIZE: usize = OK_N * std::mem::size_of::<OkD>() * OK_S;
        const MEM_SIZE: VAddr = (3 * CACHE_SIZE) as VAddr;

        // Allocate a memory area using known good parameters
        let mut mem = Mem::new::<TestCacheline<D, S>, D, S>(MEM_SIZE);
        let mut scrub_areas = Vec::<MemArea<VAddr>>::new();
        scrub_areas.push(mem.scrub_area);

        println!(
            "Test params <{}, {}, {}, {}>",
            N,
            W,
            std::mem::size_of::<D>(),
            S
        );
        let t1: TestCacheline<D, S>;
        let t2: TestCachelineData<D, S>;
        let t3: TestCacheline<D, S>;
        let t4: TestCache<
            TestCacheline<D, S>,
            TestCachelineData<D, S>,
            N,
            W,
            D,
            S,
            MemArea<VAddr>,
        >;
        let t5: TestMemoryScrubber<
            TestCache<
                TestCacheline<D, S>,
                TestCachelineData<D, S>,
                N,
                W,
                D,
                S,
                MemArea<VAddr>,
            >,
            TestCacheline<D, S>,
            TestCachelineData<D, S>,
            N,
            W,
            D,
            S,
            MemArea<VAddr>,
        >;

        let cl_size = TestCacheline::<D, S>::cacheline_size();
        let one_size = TEST_CACHE_LINES * TEST_CACHE_NUM_TOUCHED * cl_size
            as VAddr;
        let sizes = [one_size, one_size, one_size];
        // What should I be doing with test_cache?
        let (test_cache, scrub_areas) = setup_test_desc_areas::<
            TestCache<
                'a,
                TestCacheline<D, S>,
                TestCachelineData<D, S>,
                N,
                W,
                D,
                S,
                MemArea<VAddr>,
            >,
            TestCacheline<D, S>,
            TestCachelineData<D, S>,
            N,
            W,
            D,
            S,
            MemArea<VAddr>,
        >(&sizes);

        let t6 = TestMemoryScrubber::<
            TestCache<
                TestCacheline<D, S>,
                TestCachelineData<D, S>,
                N,
                W,
                D,
                S,
                MemArea<VAddr>,
            >,
            TestCacheline<D, S>,
            TestCachelineData<D, S>,
            N,
            W,
            D,
            S,
            MemArea<VAddr>,
        >::new;
        let memory_scrubber = t6(&scrub_areas);
    }

    // Check that a problem with a given Mem scrub area is properly detected
    //
    // mem: the Mem with bad scrub scrub area
    // err: the expected Error value
    fn check_scrub_area_error(mem: Mem, err: Error) {
        let mut scrub_areas = Vec::<MemArea<VAddr>>::new();
        scrub_areas.push(mem.scrub_area);
        let memory_scrubber = TestMemoryScrubber::<
            TestCache<
                TestCacheline<OkD, OK_S>,
                TestCachelineData<OkD, OK_S>,
                OK_N,
                OK_W,
                OkD,
                OK_S,
            >,
            TestCacheline<OkD, OK_S>,
            TestCachelineData<OkD, OK_S>,
            OK_N,
            OK_W,
            OkD,
            OK_S,
        >::new(&scrub_areas);

        assert!(memory_scrubber.is_err());
        assert_eq!(memory_scrubber.err().unwrap(), err);
    }

    // Verify that an error is returned if the starting address is not
    // aligned on a cache line boundary
    #[test]
    fn test_unaligned_start() {
        let mut mem = Mem::new::<TestCacheline<OkD, OK_S>, OkD, OK_S>(
            BASIC_MEM_SIZE,
        );
        mem.scrub_area.start() += 1;
        check_scrub_area_error(mem, Error::UnalignedStart);
    }

    // Verify that an error is returned if the ending address is not
    // aligned on a cache line boundary
    #[test]
    fn test_unaligned_end() {
        let mut mem = Mem::new::<TestCacheline<OkD, OK_S>, OkD, OK_S>(
            BASIC_MEM_SIZE,
        );
        mem.scrub_area.end += 1;
        check_scrub_area_error(mem, Error::UnalignedEnd);
    }

    // Verify that an error is returned if there are no areas defined
    #[test]
    fn test_null_areas() {
        let scrub_areas = Vec::<MemArea<VAddr>>::new();
        let memory_scrubber = TestMemoryScrubber::<
            TestCache<
                TestCacheline<OkD, OK_S>,
                TestCachelineData<OkD, OK_S>,
                OK_N,
                OK_W,
                OkD,
                OK_S,
            >,
            TestCacheline<OkD, OK_S>,
            TestCachelineData<OkD, OK_S>,
            OK_N,
            OK_W,
            OkD,
            OK_S,
        >::new(&scrub_areas);
        assert!(memory_scrubber.is_err());
        assert_eq!(memory_scrubber.err().unwrap(), Error::NoMemAreas);
    }

    // Verify that an error is returned if the size is zero.
    #[test]
    fn test_zero_size() {
        let mut mem = Mem::new::<TestCacheline<OkD, OK_S>, OkD, OK_S>(
            BASIC_MEM_SIZE,
        );
        mem.scrub_area.end = mem.scrub_area.start();
        check_scrub_area_error(mem, Error::EmptyMemArea);
    }

    // Verify that a small scrub with good parameters can be done.
    #[test]
    fn test_aligned() {
        const cacheline_data: TestCachelineData<OkD, OK_S> =
            TestCachelineData::<OkD, OK_S> { data: [0; OK_S] };

        const cacheline: TestCacheline<OkD, OK_S> =
            TestCacheline::<OkD, OK_S> {
                _marker1: PhantomData,
            };

        let cache: TestCache<
            TestCacheline<OkD, OK_S>,
            TestCachelineData<OkD, OK_S>,
            OK_N,
            OK_W,
            OkD,
            OK_S,
        > = TestCache::<
            TestCacheline<OkD, OK_S>,
            TestCachelineData<OkD, OK_S>,
            OK_N,
            OK_W,
            OkD,
            OK_S,
        >::new();

        let cacheline_size = TestCacheline::<OkD, OK_S>::cacheline_size();
        let cache_lines = CacheBase::cache_lines(&cache) as VAddr;
        let alloc_size = cacheline_size * cache_lines * 14;
        let mut mem =
            Mem::new::<TestCacheline<OkD, OK_S>, OkD, OK_S>(alloc_size);
        let mut scrub_areas = Vec::<MemArea<VAddr>>::new();
        scrub_areas.push(mem.scrub_area);
        let mut memory_scrubber = TestMemoryScrubber::<
            TestCache<
                TestCacheline<OkD, OK_S>,
                TestCachelineData<OkD, OK_S>,
                OK_N,
                OK_W,
                OkD,
                OK_S,
            >,
            TestCacheline<OkD, OK_S>,
            TestCachelineData<OkD, OK_S>,
            OK_N,
            OK_W,
            OkD,
            OK_S,
        >::new(&scrub_areas);
        assert!(memory_scrubber.is_ok());
        let scrubber = memory_scrubber.unwrap();
        let res = scrubber.scrub(cacheline_size * 10);

        if let Err(e) = res {
            panic!("scrub failed: {}", e);
        }
    }

    // Verify that all specified locations are scrubbed and locations outside
    // the requested are are not touched.
    #[test]
    fn test_touch_zero() {
        let cacheline_size = TestCacheline::<OkD, OK_S>::cacheline_size();
        let first_area = 0;
        test_scrubber(&[cacheline_size * TEST_SANDBOX_SIZE], first_area);
    }

    #[test]
    fn test_touch_one() {
        let cacheline_size = TestCacheline::<OkD, OK_S>::cacheline_size();
        let first_area = cacheline_size;
        test_scrubber(&[cacheline_size * TEST_SANDBOX_SIZE], first_area);
    }

    #[test]
    fn test_touch_many() {
        const MANY: usize = 50;
        let cacheline_size = TestCacheline::<OkD, OK_S>::cacheline_size();
        let first_area = cacheline_size * MANY;
        test_scrubber(&[cacheline_size * TEST_SANDBOX_SIZE], first_area);
    }

    #[test]
    fn test_touch_all() {
        let cacheline_size = TestCacheline::<OkD, OK_S>::cacheline_size();
        let first_area = cacheline_size * TEST_SANDBOX_SIZE;
        test_scrubber(&[cacheline_size * TEST_SANDBOX_SIZE], first_area);
    }

    #[test]
    fn test_touch_double_all() {
        let cacheline_size = TestCacheline::<OkD, OK_S>::cacheline_size();
        let first_area = 2 * cacheline_size * TEST_SANDBOX_SIZE;
        test_scrubber(&[cacheline_size * TEST_SANDBOX_SIZE], first_area);
    }

    #[test]
    fn test_touch_more_many() {
        const MANY: usize = 72;
        let cacheline_size = TestCacheline::<OkD, OK_S>::cacheline_size();
        let first_area = 5 * cacheline_size * (TEST_SANDBOX_SIZE + MANY);
        test_scrubber(&[cacheline_size * TEST_SANDBOX_SIZE], first_area);
    }

    #[test]
    fn test_touch_multiple_areas() {
        const MANY: usize = 72;
        let cacheline_size = TestCacheline::<OkD, OK_S>::cacheline_size();
        let first_area = 2 * cacheline_size * (TEST_SANDBOX_SIZE + MANY);
        let second_area = cacheline_size * TEST_SANDBOX_SIZE;
        let third_area = cacheline_size * MANY;
        let scrub_areas = [first_area, second_area, third_area];
        test_scrubber(&scrub_areas, first_area);
    }

    /*
        #[test]
        fn test_big() {
            const MEM_AREA_SIZE: usize = 1 * 1024 * 1024 * 1024;

            let cl: TestCachelineBase /* 32 */;
            //let mut basic_cache = BASIC_CACHE_DESC.clone();
            //let cache = &mut basic_cache;
            let cache = TestCache::<
                TestCacheline<OkD, OK_S>,
                TestCachelineData<OkD, OK_S>,
                 OK_N, OK_W, OkD, OK_S>::new();
            let mem = Mem::new::<TestCacheline>(MEM_AREA_SIZE);
            let mut scrub_areas = Vec::<MemArea<VAddr>>::new();
            scrub_areas.push(mem.scrub_area);
            let mut scrubber = TestMemoryScrubber::<
                TestCache<
                    TestCacheline<OkD, OK_S>,
                    TestCachelineData<OkD, OK_S>,
                    OK_N, OK_W, OkD, OK_S
                >,
                TestCacheline<OkD, OK_S>,
                TestCachelineData<OkD, OK_S>,
                OK_N, OK_W, OkD, OK_S
            >::new(&scrub_areas).unwrap();

    //        let mut scrubber =
    //            match MemoryScrubber::<TestCacheBase, TestCacheline>::
    //            new(cache, &scrub_areas) {
    //            Err(e) => panic!("Could not create MemoryScrubber: {}",
    //                e),
    //            Ok(scrubber) => scrubber,
    //        };

            // Use the first scrub to page in all memory
            match scrubber.scrub(MEM_AREA_SIZE) {
                Err(e) => panic!("Scrub failed: {}", e),
                Ok(_) => {},
            }

            println!("Please wait while timing scrub operation");
            let start_time = Instant::now();

            match scrubber.scrub(MEM_AREA_SIZE) {
                Err(e) => panic!("Scrub failed: {}", e),
                Ok(_) => {},
            }

            let end_time = start_time.elapsed();
            let duration = end_time.as_secs_f64();

            let mem_size = (MEM_AREA_SIZE as f64) / 1e9;
            println!("Scrub rate: {:.2} GBps", mem_size / duration);
        }

    /* FIXME: figure this out
        #[test] #[ignore]
        fn test_autoscrub() {
            let cl: TestCachelineBase /* 32 */;
            let one_size = TEST_CACHE_LINES * TEST_CACHE_NUM_TOUCHED * cl.size()
                as VAddr;
            let single_scan = one_size / 2;
            let total_scan = single_scan * 4 + 3 * cl.size();

            struct TestAutoScrubDesc {
                count:      VAddr,
                scrub_size: VAddr,
            }

            impl<'a, CD: TestCacheBaseTrait<'a, CL, D>, CL: TestCachelineBase<'a, D>,
                D: TestCacheData<D> + Index<usize>>
                AutoScrubDesc<CD, CL>
                for TestAutoScrubDesc {
                fn next(&mut self) -> VAddr {
                    let n = if self.count > self.scrub_size { self.scrub_size }
                        else { self.count };
                    self.count -= n;
                    n
                }
            }

            let sizes = [one_size, one_size, one_size];
            let (test_cache, scrub_areas) =
                setup_test_desc_areas::<TestCachelineBase /* 32 */>(&sizes);

            let mut autoscrub_desc = TestAutoScrubDesc {
                count: total_scan,
                scrub_size: single_scan,
            };

            let mut autoscrub = match AutoScrub::new(&test_cache, &scrub_areas,
                &mut autoscrub_desc) {
                Err(e) => panic!("AutoScrub::new failed: {}", e),
                Ok(autoscrub) => autoscrub,
            };
            match autoscrub.scrub() {
                Err(e) => panic!("autoscrub() failed: {}", e),
                Ok(_) => {},
            };
            verify_scrub(&autoscrub.scrubber, total_scan);
        }
    */
        // Test iterators
        const ITER_CACHE_INDEX_WIDTH: usize = 4;

        struct IterCacheline<D, const S: usize>
        where
            D:  Num,
        {
            data: [D; S],
        }

        impl<D, const S: usize>
        TestCachelineBase<D, S>
        for IterCacheline<D, S>
        where
            D: Num,
        {
        }

        #[derive(Clone)]
        struct IterCacheBase {
            cache_index_width: usize,
        }

        impl IterCacheBase {
            fn new(cache_index_width: usize) -> IterCacheBase {
                IterCacheBase {
                    cache_index_width: cache_index_width,
                }
            }
        }

        impl<'a, CL, CLD, const N: usize, const W: usize, D, const S: usize>
        CacheBase<CL, CLD, N, W, D, S>
        for IterCacheBase
        where
            CL: IterCacheline<'a, D, S>,
            D:  Num,
        {
        }

        impl<CL, CLD, const N: usize, const W: usize, D, const S: usize>
        CacheBase<CL, CLD, N, W, D, S>
        for IterCacheBase {
    /*
            fn cacheline_width(&self) -> usize {
                unimplemented!();
            }
    */

            fn cache_index_width(&self) -> usize {
                self.cache_index_width
            }

    /*
            fn read_cacheline<'a>(&self,
                _cacheline_ptr: *const dyn CachelineBase<'a>) {
    // FIXME: add more checks
                // Assure that IterCacheline::data is read
                unsafe {
                    assert_eq!((*_cacheline_ptr).data[0], (*_cacheline_ptr).data[0]);
                }
            }
    */
        }

        #[test] #[ignore]
        fn test_iter_scrublines() {
            let iter_cache = IterCacheBase::new(ITER_CACHE_INDEX_WIDTH);

            let cl_width = iter_cache.cacheline_width();
            let cl_size = 1 << cl_width;
            let ci_width = iter_cache.cache_index_width();
            let c_size = cl_size * (1 << ci_width);

            // Create areas that increasingly, then decreasingly, overlap in their
            // cache indices.
            let mut scrub_areas = Vec::<MemArea<VAddr>>::new();
            for i in 0..5 {
                let offset = i * cl_size;
                let size = offset + 3 * cl_size;
                let mut mem = Mem::new_aligned(size, c_size);
                mem.scrub_area.start() =
                    (mem.scrub_area.start() as usize + offset) as *const u8;
                scrub_areas.push(mem.scrub_area);
            }

            let mut b = Vec::<usize>::new();
            for scrub_area in &scrub_areas {
                b.push(scrub_area.start() as usize);
            }

            // We have to have enough cache lines available to avoid wrapping
            // when we go through the memory areas
            assert!(iter_cache.cache_lines() > scrub_areas.len());

            let expected_overlapping_areas_slice = [
                // Overlapping areas
                b[0] + 0 * cl_size,
                b[0] + 1 * cl_size, b[1] + 0 * cl_size,
                b[0] + 2 * cl_size, b[1] + 1 * cl_size, b[2] + 0 * cl_size,
                b[1] + 2 * cl_size, b[2] + 1 * cl_size, b[3] + 0 * cl_size,
                b[2] + 2 * cl_size, b[3] + 1 * cl_size, b[4] + 0 * cl_size,
                b[3] + 2 * cl_size, b[4] + 1 * cl_size,
                b[4] + 2 * cl_size,
            ];
            let expected_overlapping_areas =
                expected_overlapping_areas_slice.to_vec();

            let mut expecteds = Vec::<usize>::new();
            expecteds.append(&mut expected_overlapping_areas.clone());
            expecteds.append(&mut expected_overlapping_areas.clone());

            let mut iterator = ScrubAreasIterator::new(&iter_cache,
                &scrub_areas).expect("ScrubAreasIterator failed");

            for (i, expected) in (0..expecteds.len()).into_iter()
                .zip(expecteds.iter()) {
                let expected_p = *expected as *const IterCacheline;
                let actual = match iterator.next() {
                    None => panic!("Ran out of scrub areas too soon"),
                    Some(actual) => actual,
                };
                let cache_index =
                    iter_cache.cache_index(actual as *const u8);
                if actual != expected_p {
                    println!("{}: actual {:p} expected {:p} index {}", i, actual, expected_p, cache_index);
                    let actual_breakdown = breakdown(&iter_cache,
                            actual as *const IterCacheline, &scrub_areas, &b);
                    let expected_breakdown = breakdown(&iter_cache,
                        expected_p, &scrub_areas, &b);
                    eprintln!("   actual: b[{}] + {} * cl_size expected: b[{}] + {} * cl_size",
                        actual_breakdown.0, actual_breakdown.1 / cl_size,
                        expected_breakdown.0, expected_breakdown.1 / cl_size);

                    print_scrub_areas(&iter_cache, &scrub_areas);
                }
                assert_eq!(actual, expected_p);
            }
            let next = iterator.next();
            assert!(next.is_some());
        }

        // Find the base and offset within the scrub areas
        fn breakdown(iter_cache: &IterCacheBase,
            expected: *const IterCacheline<OkD, OK_S>, scrub_areas: &Vec::<MemArea<VAddr>>,
            b: &Vec::<usize>) -> (usize, usize) {
            let mut found: Option<usize> = None;
            for (j, scrub_area) in (0..scrub_areas.len()).into_iter()
                .zip(scrub_areas.iter()) {
                if ((scrub_area.start() as usize) <= expected as usize) &&
                    ((expected as usize) < scrub_area.end as usize) {
                    found = Some(j);
                    break;
                }
            }
            if found.is_none() {
                print_scrub_areas(&iter_cache, &scrub_areas);
                panic!("Unable to find address {:p}", expected);
            }
            let j = found.unwrap();
            let delta = expected as usize - b[j];
            (j, delta)
        }

        #[test] #[ignore]
        fn test_iter_cache_indices() {

            let iter_cache = IterCacheBase::new(ITER_CACHE_INDEX_WIDTH);

            let cl_width = iter_cache.cacheline_width();
            let cl_size = 1 << cl_width;
            let ci_width = iter_cache.cache_index_width();
            let c_size = cl_size * (1 << ci_width);

            let mut scrub_areas = Vec::<MemArea<VAddr>>::new();
            scrub_areas.push(Mem
                ::new_aligned(4 * cl_size, c_size).unwrap().scrub_area);
            scrub_areas.push(Mem
                ::new_aligned(5 * cl_size, c_size).unwrap().scrub_area);
            scrub_areas[1].start() = (scrub_areas[1].start as usize +
                1 * cl_size) as *const u8;
            scrub_areas.push(Mem
                ::new_aligned(6 * cl_size, c_size).unwrap().scrub_area);
            scrub_areas[2].start() = (scrub_areas[2].start as usize +
                2 * cl_size) as *const u8;
            scrub_areas.push(Mem
                ::new_aligned(7 * cl_size, c_size).unwrap().scrub_area);
            scrub_areas[3].start() = (scrub_areas[3].start as usize +
                3 * cl_size) as *const u8;
            scrub_areas.push(Mem
                ::new_aligned(8 * cl_size, c_size).unwrap().scrub_area);
            scrub_areas[4].start() = (scrub_areas[4].start as usize +
                4 * cl_size) as *const u8;
            scrub_areas.push(Mem
                ::new_aligned(9 * cl_size, c_size).unwrap().scrub_area);
            scrub_areas[5].start() = (scrub_areas[5].start as usize +
                5 * cl_size) as *const u8;

            let base1 = scrub_areas[0].start() as usize;
            let base2 = scrub_areas[1].start() as usize;
            let base3 = scrub_areas[2].start() as usize;
            let base4 = scrub_areas[3].start() as usize;
            let base5 = scrub_areas[4].start() as usize;
            let base6 = scrub_areas[5].start() as usize;

            let expecteds = [
                base1 + 0 * cl_size,
                base1 + 1 * cl_size, base2 + 0 * cl_size,
                base1 + 2 * cl_size, base2 + 1 * cl_size, base3 + 0 * cl_size,
                base1 + 3 * cl_size, base2 + 2 * cl_size, base3 + 1 * cl_size,
                base4 + 0 * cl_size, base2 + 3 * cl_size, base3 + 2 * cl_size,
                base4 + 1 * cl_size, base5 + 0 * cl_size, base3 + 3 * cl_size,
                base4 + 2 * cl_size, base5 + 1 * cl_size,
                base6 + 0 * cl_size,
            ];

            let mut iterator = CacheIndexIterator::new(&iter_cache,
                &scrub_areas).expect("CacheIndexIterator::new failed");

            for expected in expecteds {
                let expected_p = expected as *const IterCacheline;
                let actual = match iterator.next() {
                    None => panic!("Ran out of scrub areas too soon"),
                    Some(actual) => actual,
                };
                assert_eq!(actual, expected_p);
            }
            let next = iterator.next();
            assert!(next.is_some());
        }

        #[test] #[ignore]
        fn test_iter_scrub_areas() {
            struct TestDesc<'a> {
                cur_index:  usize,
                expected:   &'a [usize],
            }

            let iter_cache = IterCacheBase::new(ITER_CACHE_INDEX_WIDTH);

            let cl_width = iter_cache.cacheline_width();
            let cl_size = 1 << cl_width;
            let ci_width = iter_cache.cache_index_width();
            let c_size = cl_size * (1 << ci_width);

            let base1 = 0x10 * c_size + 0 * cl_size;
            let s1 = (base1) as *const u8;
            let e1 = (base1 + 1 * cl_size) as *const u8;
            let base2 = 0x20 * c_size + 1 * cl_size;
            let s2 = (base2) as *const u8;
            let e2 = (base2 + 1 * cl_size) as *const u8;
            let base3 = 0x30 * c_size + 2 * cl_size;
            let s3 = (base3) as *const u8;
            let e3 = (base3 + 4 * c_size + 3 * cl_size) as *const u8;
            let base4 = 0x40 * c_size + 3 * cl_size;
            let s4 = (base4) as *const u8;
            let e4 = (base4 + 2 * cl_size) as *const u8;
            let base5 = 0x50 * c_size + 3 * cl_size;
            let s5 = (base5) as *const u8;
            let e5 = (base5 + 2 * cl_size + 2 * cl_size) as *const u8;

            let phys_scrub_areas = [
                MemArea::new(s1, e1),
                MemArea::new(s2, e2),
                MemArea::new(s3, e3),
                MemArea::new(s4, e4),
                MemArea::new(s5, e5),
            ];

            let expected1 = [base1 + 0 * c_size];
            let expected2 = [base1 + 0 * c_size + 1 * cl_size,
                base2 + 0 * c_size + 0 * cl_size];
            let expected3 = [base2 + 0 * c_size + 1 * cl_size, base3 + 0 * c_size,
                base3 + 1 * c_size, base3 + 2 * c_size, base3 + 3 * c_size];

            let test_desc:  [TestDesc; 3] = [
                TestDesc {cur_index: 0, expected: &expected1},
                TestDesc {cur_index: 1, expected: &expected2},
                TestDesc {cur_index: 2, expected: &expected3},
            ];

            for test_datum in test_desc {
                let iterator = MemAreasIterator::new(&iter_cache,
                    &phys_scrub_areas, test_datum.cur_index).
                    expect("MemAreasIterator failed");

                // Scan the entire set of MemAreas
                let mut count = 0;
                for (actual, expected) in
                    iterator.into_iter().zip(test_datum.expected.iter()) {
                    let expected_p = *expected as *const IterCacheline;
                    assert_eq!(actual, expected_p);
                    count += 1;
                };
                assert_eq!(count, test_datum.expected.len());
            }
        }

        #[test] #[ignore]
        fn test_iter_scrub_area() {
            struct TestDesc<'a> {
                cur_index:  usize,
                scrub_area: &'a MemArea<VAddr>,
                expected:   &'a[usize],
            }
            let iter_cache = IterCacheBase::new(ITER_CACHE_INDEX_WIDTH);

            let cl_width = iter_cache.cacheline_width();
            let cl_size = 1 << cl_width;
            let ci_width = iter_cache.cache_index_width();
            let c_size = cl_size * (1 << ci_width);

            // These all start two cache lines above a cache-sized boundary
            let delta1 = 1 * cl_size;
            let mut phys_mem1 = Mem::new_aligned(delta1 + 2 * cl_size, c_size);
            phys_mem1.scrub_area.start() = (phys_mem1.scrub_area.start as usize +
                delta1) as *const u8;

            let delta2 = 1 * cl_size;
            let mut phys_mem2 = Mem::new_aligned(delta2 + 3 * c_size, c_size);
            phys_mem2.scrub_area.start() = (phys_mem2.scrub_area.start as usize +
                  delta2) as *const u8;

            let delta3 = 1 * cl_size;
            let mut phys_mem3 = Mem::new_aligned(delta3 + 1 * cl_size, c_size);
            phys_mem3.scrub_area.start() = (phys_mem3.scrub_area.start as usize +
                  delta3) as *const u8;

            let base1 = phys_mem1.scrub_area.start() as usize;
            let base2 = phys_mem2.scrub_area.start() as usize;
            let base3 = phys_mem3.scrub_area.start() as usize;

            let expected1 = [];
            let expected2 = [base1];
            let expected3 = [base1 + cl_size];
            let expected4 = [base2 + (c_size - delta2) + 0 * c_size + 0 * cl_size,
                base2 + (c_size - delta2) + 1 * c_size + 0 * cl_size];
            let expected5 = [base2 + 0 * c_size + 0 * cl_size,
                base2 + 1 * c_size + 0 * cl_size,
                base2 + 2 * c_size + 0 * cl_size];
            let expected6 = [base2 + 0 * c_size + 1 * cl_size,
                base2 + 1 * c_size + 1 * cl_size,
                base2 + 2 * c_size + 1 * cl_size];
            let expected7 = [base3 + 0 * cl_size];

            let test_desc: [TestDesc; 7] = [
                TestDesc { cur_index: 0, scrub_area: &phys_mem1.scrub_area.clone(),
                    expected: &expected1},
                TestDesc { cur_index: 1, scrub_area: &phys_mem1.scrub_area.clone(),
                    expected: &expected2},
                TestDesc { cur_index: 2, scrub_area: &phys_mem1.scrub_area.clone(),
                    expected: &expected3},
                TestDesc { cur_index: 0, scrub_area: &phys_mem2.scrub_area.clone(),
                    expected: &expected4},
                TestDesc { cur_index: 1, scrub_area: &phys_mem2.scrub_area.clone(),
                    expected: &expected5},
                TestDesc { cur_index: 2, scrub_area: &phys_mem2.scrub_area.clone(),
                    expected: &expected6},
                TestDesc { cur_index: 1, scrub_area: &phys_mem3.scrub_area.clone(),
                    expected: &expected7},
            ];


            for test_datum in test_desc {
                let sax = &test_datum.scrub_area;
                let cur_index = test_datum.cur_index;
                let iterator = MemAreaIterator::new(&iter_cache, sax,
                    cur_index).expect("MemAreaIterator::new failed");

                // Scan the entire set of MemAreas
                let mut count = 0;
                for (actual, expected) in
                    iterator.into_iter().zip(test_datum.expected.iter()) {
                    let expected_p = *expected as *const IterCacheline;
                    assert_eq!(actual, expected_p);
                    count += 1;
                };
                assert_eq!(count, test_datum.expected.len());
            }
        }

        #[allow(dead_code)]
        fn print_scrub_areas(cache: &IterCacheBase,
            scrub_areas: &[MemArea]) {
            let mut total_bytes: usize = 0;
            let cacheline_width = cache.cacheline_width();

            // Print the tuples in the vector
            println!("Physical addresses:");
            for (i, scrub_area) in (0..scrub_areas.len()).into_iter()
                .zip(scrub_areas.iter()) {
                let delta_cachelines =
                    cache.size_in_cachelines(&scrub_area);
                let delta_bytes = delta_cachelines << cacheline_width;
                total_bytes += delta_bytes;
                println!("{}: {:p}-{:p}: {} ({} cache lines)", i,
                    scrub_area.start(), scrub_area.end, delta_bytes,
                    delta_cachelines);
            }

            println!("total size {} bytes ({} cache lines)", total_bytes,
                total_bytes >> cacheline_width);
        }

        #[test] #[ignore]
        fn test_iter() {
            let iter_cache = IterCacheBase::new(ITER_CACHE_INDEX_WIDTH);

            let cl_width = iter_cache.cacheline_width();
            let cl_size = 1 << cl_width;
            let ci_width = iter_cache.cache_index_width();
            let c_size = cl_size * (1 << ci_width);

            let s1 = (2 * cl_size) as * const u8;
            let e1 = (2 * cl_size + 3 * c_size) as *const u8;
            let s2 = (4 * c_size) as *const u8;
            let e2 = (4 * c_size + cl_size) as *const u8;
            let s3 = (5 * c_size) as *const u8;
            let e3 = (5 * c_size + 2 * cl_size) as *const u8;
            let virt_scrub_areas = [
                MemArea::new(s1, e1),
                MemArea::new(s2, e2),
                MemArea::new(s3, e3),
            ].to_vec();

            let mut total_bytes: usize = 0;
            for scrub_area in &virt_scrub_areas {
                let delta = iter_cache.size_in_cachelines(&scrub_area);
                total_bytes += delta;
            }
            total_bytes *= iter_cache.cacheline_size();

            // Print the tuples in the vector
            let cur_indices = [0];

            for _cur_index in cur_indices {
                let mut iterator = ScrubAreasIterator::new(&iter_cache,
                    &virt_scrub_areas).expect("ScrubAreasIterator failed");

                // Scan the entire set of MemAreas
                for _i in 0..total_bytes >> iter_cache.cacheline_width() {
                    let next = iterator.next();
                    let _p = next.unwrap();
    // FIXME: check against actuals
                };
            }
        }
    */

    // Test support function that scrubs a section of memory, then verifies
    // that things were properly touch or not touched.
    // sizes - array of sizes of memory areas to scrub
    // n - number of cache lines to scrub
    fn test_scrubber(sizes: &[usize], n: usize) {
        println!("In test_scrubber");
        let (test_cache, scrub_areas) = setup_test_desc_areas::<
            TestCache<
                TestCacheline<OkD, OK_S>,
                TestCachelineData<OkD, OK_S>,
                OK_N,
                OK_W,
                OkD,
                OK_S,
            >,
            TestCacheline<OkD, OK_S>,
            TestCachelineData<OkD, OK_S>,
            OK_N,
            OK_W,
            OkD,
            OK_S,
        >(sizes);

        let mut memory_scrubber = TestMemoryScrubber::<
            TestCache<
                TestCacheline<OkD, OK_S>,
                TestCachelineData<OkD, OK_S>,
                OK_N,
                OK_W,
                OkD,
                OK_S,
            >,
            TestCacheline<OkD, OK_S>,
            TestCachelineData<OkD, OK_S>,
            OK_N,
            OK_W,
            OkD,
            OK_S,
        >::new(&scrub_areas)
        .unwrap();

        if let Err(e) = memory_scrubber.scrub(n) {
            panic!("scrub failed: {}", e);
        };

        memory_scrubber.verify();
    }

    // Set up a TestCacheBase and MemAreas
    fn setup_test_desc_areas<
        'a,
        C,
        CL,
        CLD,
        const N: usize,
        const W: usize,
        D,
        const S: usize,
        M,
    >(
        sizes: &[usize],
    ) -> (TestCache<CL, CLD, N, W, D, S>, Vec<MemArea<VAddr>>)
    where
        C: TestCacheBase<'a, CL, CLD, N, W, D, S>,
        CL: TestCachelineBase<D, S>,
        CLD: TestCachelineDataBase<D, S>,
        D: Num + 'a,
        M: MemAreaBase<VAddr>,
    {
        // FIXME: this had the argument sizes, why?
        let test_cache = TestCache::<CL, CLD, N, W, D, S>::new(sizes);

        // Allocate memory areas according to the given sizes
        let mut scrub_areas: Vec<MemArea<VAddr>> = vec![];
        {
            let cacheline_ptr: *const CLD;

            for read_info in &test_cache.read_infos {
                scrub_areas.push(read_info.mem.scrub_area.clone());
            }
        }

        (test_cache, scrub_areas)
    }

    /// Verify the proper locations were hit
    ///
    /// * `scrubber` - MemoryScrubber to use
    ///
    /// * `n` - Bytes scrubbed
    ///
    /// This essentially reimplements the iterator code but in a more straight-
    /// forward way so that the two implementations can verify each other.
    fn verify_scrub<
        'a,
        C,
        CL,
        CLD,
        const N: usize,
        const W: usize,
        D,
        const S: usize,
        M,
    >(
        scrubber: &TestMemoryScrubber<'a, C, CL, CLD, N, W, D, S, M>,
        n: usize,
    ) where
        C: TestCacheBase<'a, CL, CLD, N, W, D, S>,
        CL: TestCachelineBase<D, S>,
        CLD: TestCachelineDataBase<D, S>,
        D: Num + 'a,
        M: MemAreaBase<VAddr>,
    {
        let cache = scrubber.cache();
        let cacheline_width = CL::cacheline_width();
        let n_in_cachelines = n >> cacheline_width;
        let cache_lines = cache.cache_lines();

        // Count the total number of scrub lines in all of the MemAreas
        let mut scrub_lines = 0;

        unimplemented!();
        /*
                for scrub_area in scrubber.iterator.scrub_areas {
                    scrub_lines += C::size_in_cachelines(scrub_area);
                }
        */

        unimplemented!();
        /*
                let n_min_reads = match (n_in_cachelines / scrub_lines)
                    .try_into() {
                    Err(e) => panic!("Internal Error: n_min_reads conversion failed: {}", e),
                    Ok(n_min_reads) => n_min_reads,
                };
        */
        let n_extra_reads = n_in_cachelines % scrub_lines;

        let mut verified = 0;

        /* FIXEME: restore this
                // Scan the cache indices
                for cache_index in 0..cache_lines {
                    verify_cache_index::<TestCacheBaseTrait>(&scrubber.cache(),
                        cache_index, n_min_reads, n_extra_reads, &mut verified);
                }
        */
    }

    // Verified that all of the areas for this cache line are correct
    // memory_scrubber: Memory scrubber
    // cache_index:     Cache index being verified
    // verified:        Number of cache indices verified so far
    //    fn verify_cache_index<CD: TestCacheBase<'a, CL>, CL: TestCachelineBase>(cache: &C,
    fn verify_cache_index<C>(
        cache: &C,
        cache_index: usize,
        n_min_reads: usize,
        n_extra_reads: usize,
        verified: &mut usize,
    ) {
        /*
        unimplemented!();
                for read_info in cache.test.borrow_mut().read_infos {
                    verify_read_info(cache, &read_info, cache_index,
                        n_min_reads, n_extra_reads, verified);
                }
        */
    }

    // Verify that one scrub area is correct
    // memory_scrubber: The MemoryScrubber being verified
    // scrub_area: the area being verified
    // verified: The number of addresses that have already been verified
    // n_min_reads: The minimum number of reads in the vectors in n_reads
    // n_extra_reads: The number of items in the vectors in n_reads which
    //      are greater than n_min_reads by one
    //
    // Returns the number of addresses that were verified in this scrub area
    fn verify_read_info<
        'a,
        CL,
        CLD,
        const N: usize,
        const W: usize,
        D,
        const S: usize,
    >(
        cache: &dyn TestCacheBase<'a, CL, CLD, N, W, D, S>,
        read_info: &ReadInfo,
        cache_index: usize,
        n_min_reads: usize,
        n_extra_reads: usize,
        verified: &mut usize,
    ) where
        CL: TestCachelineBase<D, S>,
        CLD: TestCachelineDataBase<D, S>,
        D: Num,
    {
        let cache_lines = cache.cache_lines();
        /*
                let size_in_cachelines =
                    cache.size_in_cachelines(&read_info.mem.scrub_area);
        */
        let size_in_cachelines = 1;
        unimplemented!();

        let start = read_info.mem.scrub_area.start();
        unimplemented!();
        let start_index = 0;
        /*
                let start_index =
                    cache.offset_to_next_index(start, cache_index);
        */
        let mem = unsafe {
            slice::from_raw_parts(start as *const CL, size_in_cachelines)
        };

        let n_reads = &read_info.n_reads.as_ref().unwrap();
        verify_guard(n_reads, 0);

        // Now verify the contents of the memory to see whether they were
        // touched the expected number of times. The number of hits for a
        // location i in n_reads[] will be at least equal to the number of
        // complete scans of the memory area. Then, the remaining number of
        // items in the scan will be one larger.
        for i in (start_index..size_in_cachelines).step_by(cache_lines) {
            let inc = if verified < &mut n_extra_reads { 1 } else { 0 };
            let n_expected = n_min_reads + inc;
            let n_actual = n_reads[GUARD_LINES + i];
            println!(
                "n_extra_reads {} n_min_reads {}",
                n_extra_reads, n_min_reads
            );
            println!("n_expected {} n_actual {}", n_expected, n_actual);
            println!("i {}", i);
            let n_expected: NRead = n_expected
                .try_into()
                .expect("Numerical conversion failed");
            assert_eq!(n_actual, n_expected);

            unimplemented!();
            let mem_actual = 0;
            /*
                        let mem_actual = mem[i].data[0];
            */
            let mem_expected =
                if n_min_reads == 0 && *verified >= n_extra_reads {
                    0
                } else {
                    TEST_COOKIE
                };

            assert_eq!(mem_actual, mem_expected);
            unimplemented!();
            /*
                        for data in &mem[i].data[1..] {
                            let mem_actual = *data;
                            assert_eq!(mem_actual, 0);
                        }
            */

            *verified += 1;
        }

        verify_guard(n_reads, GUARD_LINES + TEST_SANDBOX_SIZE);
    }

    // Verify a guard area before the memory area. This should
    // not have been seen and so should have a zero value
    // n_reads - Array that should have all zero values at the offset for
    //      GUARD_LINES elements
    // offset - Offset in n_reads to check
    fn verify_guard(n_reads: &Vec<NRead>, offset: usize) {
        for i in 0..GUARD_LINES {
            let actual = n_reads[offset + i];
            assert_eq!(actual, 0);
        }
    }
*/
}
