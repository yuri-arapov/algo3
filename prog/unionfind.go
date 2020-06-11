// Union-Find data structure.
// based on https://github.com/korzhnev/unionfind
// depend on utils.go

package main

type UnionFind struct {
	id     []int // id of i-th group
	size   []int // size of i-th group
	volume int   // number of groups
}

// create new UnionFind of given size
func NewUnionFind(sz int) *UnionFind {
	PanicIf(sz < 0, "bad argument: %d", sz)
	id := make([]int, sz)
	size := make([]int, sz)
	for i, _ := range id {
		id[i] = i
		size[i] = 1
	}
	return &UnionFind{id: id, size: size, volume: sz}
}

// Check if nodes x and y are connected
func (uf *UnionFind) Connected(x, y int) bool {
	uf.checkRange(x, y)
	return uf.parent(x) == uf.parent(y)
}

// Merge nodes x and y into single cluster
func (uf *UnionFind) Union(x, y int) {
	uf.checkRange(x, y)
	px := uf.parent(x)
	py := uf.parent(y)
	if px != py {
		uf.volume--
		if uf.size[px] < uf.size[py] {
			uf.id[px] = py
			uf.size[py] += uf.size[px]
		} else {
			uf.id[py] = px
			uf.size[px] += uf.size[py]
		}
	}
}

func (uf *UnionFind) Volume() int {
	return uf.volume
}

func (uf *UnionFind) parent(x int) int {
	for x != uf.id[x] {
		// path compression
		uf.id[x] = uf.id[uf.id[x]]
		x = uf.id[x]
	}
	return x
}

func (uf *UnionFind) checkRange(indexes ...int) {
	for _, i := range indexes {
		PanicIf(i < 0 || i >= len(uf.id), "out of range: %d", i)
	}
}

// end of file
