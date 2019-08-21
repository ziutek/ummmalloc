// -----------------------------------------------------------------------------
// ummmalloc.go - a memory allocator for embedded systems (microcontrollers)
//
// See LICENSE for copyright notice
// See README.md for acknowledgements and description of internals
// -----------------------------------------------------------------------------
//
// R.Hempel  2007-09-22 - Original
// R.Hempel  2008-12-11 - Added MIT License biolerplate
//                      - realloc() now looks to see if previous block is free
//                      - made common operations functions
// R.Hempel  2009-03-02 - Added macros to disable tasking
//                      - Added function to dump heap and check for valid free
//                         pointer
// R.Hempel  2009-03-09 - Changed name to umm_malloc to avoid conflicts with
//                        the mm_malloc() library functions
//                      - Added some test code to assimilate a free block
//                         with the very block if possible. Complicated and
//                         not worth the grief.
// D.Frank   2014-04-02 - Fixed heap configuration when UMM_TEST_MAIN is NOT set,
//                         added user-dependent configuration file umm_malloc_cfg.h
// R.Hempel  2016-12-04 - Add support for Unity test framework
//                      - Reorganize source files to avoid redundant content
//                      - Move integrity and poison checking to separate file
// R.Hempel  2017-12-29 - Fix bug in realloc when requesting a new block that
//                         results in OOM error - see Issue 11
// M.Derkacz 2019-08-19 - Translate to Go. Only "best fit" mode is implemented.
//
// -----------------------------------------------------------------------------

package ummmalloc

import (
	"sync"
	"unsafe"
)

const (
	_FREELIST_MASK = 0x8000
	_BLOCKNO_MASK  = 0x7FFF
)

type ptr struct {
	next uint16
	prev uint16
}

// block represents the block header or 8 bytes of data
type block struct {
	header struct{ used ptr }
	body   struct{ free ptr } // can add aditional data here for bigger block
}

// UMM can manage heap up to 256KB.
type UMM struct {
	// The heap size can contain up to 32768 blocks.
	// The first block is the head of the free block list.
	heap  []block
	mutex sync.Mutex
}

func (umm *UMM) nblock(b int) int          { return int(umm.heap[b].header.used.next) }
func (umm *UMM) setnblock(b, n int)        { umm.heap[b].header.used.next = uint16(n) }
func (umm *UMM) pblock(b int) int          { return int(umm.heap[b].header.used.prev) }
func (umm *UMM) setpblock(b, n int)        { umm.heap[b].header.used.prev = uint16(n) }
func (umm *UMM) nfree(b int) int           { return int(umm.heap[b].body.free.next) }
func (umm *UMM) setnfree(b, n int)         { umm.heap[b].body.free.next = uint16(n) }
func (umm *UMM) pfree(b int) int           { return int(umm.heap[b].body.free.prev) }
func (umm *UMM) setpfree(b, n int)         { umm.heap[b].body.free.prev = uint16(n) }
func (umm *UMM) data(b int) unsafe.Pointer { return unsafe.Pointer(&umm.heap[b].body) }
func (umm *UMM) lock()                     { umm.mutex.Lock() }
func (umm *UMM) unlock()                   { umm.mutex.Unlock() }

// Split the block `c` into two blocks: `c` and `c + blocks`.
//
// - `new_freemask` should be `0` if `c + blocks` used, or `_FREELIST_MASK`
//   otherwise.
//
// Note that free pointers are NOT modified by this function.
func (umm *UMM) splitBlock(c, blocks, newFreemask int) {
	umm.setnblock(c+blocks, umm.nblock(c)&_BLOCKNO_MASK|newFreemask)
	umm.setpblock(c+blocks, c)

	umm.setpblock(umm.nblock(c)&_BLOCKNO_MASK, c+blocks)
	umm.setnblock(c, c+blocks)
}

func (umm *UMM) disconnectFromFreeList(c int) {
	// Disconnect this block from the FREE list
	umm.setnfree(umm.pfree(c), umm.nfree(c))
	umm.setpfree(umm.nfree(c), umm.pfree(c))

	// And clear the free block indicator
	umm.setnblock(c, umm.nblock(c)&^_FREELIST_MASK)
}

// The assimilateUp() function assumes that nblock(c) does NOT
// have the _FREELIST_MASK bit set!
func (umm *UMM) assimilateUp(c int) {
	if umm.nblock(umm.nblock(c))&_FREELIST_MASK != 0 {
		// Disconnect the next block from the FREE list
		umm.disconnectFromFreeList(umm.nblock(c))

		// Assimilate the next block with this one
		umm.setpblock(umm.nblock(umm.nblock(c))&_BLOCKNO_MASK, c)
		umm.setnblock(c, umm.nblock(umm.nblock(c))&_BLOCKNO_MASK)
	}
}

// The assimilateDown() function assumes that nblock(c) does NOT
// have the _FREELIST_MASK bit set!
func (umm *UMM) assimilateDown(c, freemask int) int {
	umm.setnblock(umm.pblock(c), umm.nblock(c)|freemask)
	umm.setpblock(umm.nblock(c), umm.pblock(c))
	return umm.pblock(c)
}

// Init initializes umm. heapAddr must be aligned to machine word size.
func (umm *UMM) Init(heapAddr unsafe.Pointer, heapSize uintptr) {
	// init the heap pointer and size, and memset the whole heap to 0
	umm.heap = (*[32768]block)(heapAddr)[:heapSize/unsafe.Sizeof(block{})]
	for i := range umm.heap {
		umm.heap[i] = block{}
	}

	// setup initial blank heap structure */

	const (
		block_0th = 0 // index of the 0th `umm_block`
		block_1th = 1 // index of the 1st `umm_block`
	)

	block_last := len(umm.heap) - 1 // index of the latest `umm_block`

	// setup the 0th `umm_block`, which just points to the 1st
	umm.setnblock(block_0th, block_1th)
	umm.setnfree(block_0th, block_1th)
	umm.setpfree(block_0th, block_1th)

	// Now, we need to set the whole heap space as a huge free block. We should
	// not touch the 0th `umm_block`, since it's special: the 0th `umm_block`
	// is the head of the free block list. It's a part of the heap invariant.

	// 1th `umm_block` has pointers:
	//
	// - next `umm_block`: the latest one
	// - prev `umm_block`: the 0th
	//
	// Plus, it's a free `umm_block`, so we need to apply `UMM_FREELIST_MASK`
	//
	// And it's the last free block, so the next free block is 0.
	umm.setnblock(block_1th, block_last|_FREELIST_MASK)
	umm.setnfree(block_1th, 0)
	umm.setpblock(block_1th, block_0th)
	umm.setpfree(block_1th, block_0th)

	// latest `umm_block` has pointers:
	//
	// - next `umm_block`: 0 (meaning, there are no more `umm_blocks`)
	// - prev `umm_block`: the 1st
	//
	// It's not a free block, so we don't touch NFREE / PFREE at all.
	umm.setnblock(block_last, 0)
	umm.setpblock(block_last, block_1th)
}

// Free accpets unsafe.Pinter to force to import the unsafe package before use it.
func (umm *UMM) Free(p unsafe.Pointer) {
	ptr := uintptr(p)

	// If we're being asked to free a NULL pointer, well that's just silly!
	if ptr == 0 {
		return
	}

	// FIXME: At some point it might be a good idea to add a check to make sure
	//        that the pointer we're being asked to free up is actually within
	//        the umm_heap!
	//
	// NOTE:  See the new umm_info() function that you can use to see if a ptr is
	//        on the free list!

	// Figure out which block we're in. Note the use of truncated division...
	c := int((ptr - uintptr(unsafe.Pointer(&umm.heap[0]))) / unsafe.Sizeof(block{}))

	// Protect the critical section...
	umm.lock()

	// Now let's assimilate this block with the next one if possible.
	umm.assimilateUp(c)

	// Then assimilate with the previous block if possible
	if umm.nblock(umm.pblock(c))&_FREELIST_MASK != 0 {
		c = umm.assimilateDown(c, _FREELIST_MASK)
	} else {
		// The previous block is not a free block, so add this one to the head
		// of the free list
		umm.setpfree(umm.nfree(0), c)
		umm.setnfree(c, umm.nfree(0))
		umm.setpfree(c, 0)
		umm.setnfree(0, c)
		umm.setnblock(c, umm.nblock(c)|_FREELIST_MASK)
	}

	// Release the critical section...
	umm.unlock()
}

func (umm *UMM) blocks(size uintptr) int {
	// The calculation of the block size is not too difficult, but there are
	// a few little things that we need to be mindful of.
	//
	// When a block removed from the free list, the space used by the free
	// pointers is available for data. That's what the first calculation
	// of size is doing.

	if size <= unsafe.Sizeof(block{}.body) {
		return 1
	}

	// If it's for more than that, then we need to figure out the number of
	// additional whole blocks the size of an block are required.

	size -= 1 + unsafe.Sizeof(block{}.body)

	return int(2 + size/unsafe.Sizeof(block{}))
}

// Calloc returns uintptr to force to use of unsafe package to use allocated memory.
func (umm *UMM) Malloc(size uintptr, needzero bool) (p uintptr) {
	if size == 0 {
		return 0
	}

	blocks := umm.blocks(size)

	umm.lock()

	bestBlock := 0
	bestSize := 0x8000

	for cf := umm.nfree(0); cf != 0; cf = umm.nfree(cf) {
		blockSize := (umm.nblock(cf) & _BLOCKNO_MASK) - cf
		if blockSize >= blocks && blockSize < bestSize {
			bestBlock = cf
			bestSize = blockSize
		}
	}

	if bestBlock != 0 && umm.nblock(bestBlock)&_BLOCKNO_MASK != 0 {
		// This is an existing block in the memory heap, we just need to split off
		// what we need, unlink it from the free list and mark it as in use, and
		// link the rest of the block back into the freelist as if it was a new
		// block on the free list...

		if bestSize == blocks {
			// It's an exact fit and we don't neet to split off a block.
			// Disconnect this block from the FREE list
			umm.disconnectFromFreeList(bestBlock)
		} else {
			// It's not an exact fit and we need to split off a block.
			// split current free block `cf` into two blocks. The first one will be
			// returned to user, so it's not free, and the second one will be free.
			umm.splitBlock(bestBlock, blocks, _FREELIST_MASK)

			// `umm_split_block()` does not update the free pointers (it affects
			// only free flags), but effectively we've just moved beginning of the
			// free block from `cf` to `cf + blocks`. So we have to adjust pointers
			// to and from adjacent free blocks.

			// previous free block
			umm.setnfree(umm.pfree(bestBlock), bestBlock+blocks)
			umm.setpfree(bestBlock+blocks, umm.pfree(bestBlock))

			// next free block
			umm.setpfree(umm.nfree(bestBlock), bestBlock+blocks)
			umm.setnfree(bestBlock+blocks, umm.nfree(bestBlock))
		}
		p = uintptr(umm.data(bestBlock))
	}

	umm.unlock()

	if p != 0 && needzero {
		// clear memory
		var zero block
		umm.heap[bestBlock].body = zero.body
		for i := 1; i < blocks; i++ {
			umm.heap[bestBlock+i] = zero
		}
	}

	return
}

func (umm *UMM) BlockInfo(current int) (size int, used bool) {
	next := umm.nblock(current)
	used = next&_FREELIST_MASK == 0
	next &^= _FREELIST_MASK
	size = next - current
	return
}
