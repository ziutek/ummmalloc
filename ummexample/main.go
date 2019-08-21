package main

import (
	"fmt"
	"math/rand"
	"unsafe"

	"github.com/ziutek/ummmalloc"
)

var umm ummmalloc.UMM

func printBlocks() {
	var n rune
	current := 0
	for {
		size, used := umm.BlockInfo(current)
		if size <= 0 {
			break
		}
		current += size
		c := 'a'
		if used {
			c = 'A'
		}
		c += n
		n = (n + 1) % 26
		for i := 0; i < size; i++ {
			fmt.Printf("%c", c)
		}
	}
	fmt.Printf("\n")
}

func main() {
	heap := make([]uint64, 80)
	umm.Init(unsafe.Pointer(&heap[0]), uintptr(len(heap)*8))

	pointers := make(map[uintptr]struct{})

	for {
		rnd := uintptr(rand.Int31())
		if rnd&0x10000 == 0 {
			ptr := umm.Malloc(rnd&0xFF, true)
			if ptr != 0 {
				pointers[ptr] = struct{}{}
				printBlocks()
				continue
			}
		}
		if len(pointers) > 0 {
			for ptr := range pointers {
				delete(pointers, ptr)
				umm.Free(unsafe.Pointer(ptr))
				break
			}
			printBlocks()
		}
	}
}
