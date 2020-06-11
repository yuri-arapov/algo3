// clastering
// depends on utils.go unionfind.go
// usage:
//   go run clustering.go unionfind.go utils.go

package main

import (
	"fmt"
	"sort"
	"time"
)

type edge struct {
	from int
	to   int
	cost int
}

func ReadEdges(fname string) (int, []edge) {
	PrintDebugLvl(2, "Reading graph edges from \"%s\"", fname)
	nodeCount := 0
	edges := make([]edge, 0)
	lineNo := 0
	err := readFilePerLine(fname,
		func(line string) error {
			lineNo++
			if lineNo == 1 {
				// parse 'number_of_nodes'
				n, e := fmt.Sscanf(line, "%d", &nodeCount)
				PanicIf(n != 1 || e != nil, "failed to read %s, line %d: bad format", fname, lineNo)
			} else {
				// parse 'from to cost'
				var from, to, cost int
				n, e := fmt.Sscanf(line, "%d %d %d", &from, &to, &cost)
				PanicIf(n != 3 || e != nil, "failed to read %s, line %d: bad format", fname, lineNo)
				edges = append(edges, edge{from, to, cost})
			}
			return nil
		})
	PanicIfError(err)

	PrintDebugLvl(2, "  %d lines read, %d nodes, %d edges", lineNo, nodeCount, len(edges))

	return nodeCount, edges
}

// max spacing clustering algorithm
func MaxSpacingClustering(k int, nNodes int, edges []edge) int {
	sortedEdges := make([]edge, len(edges))
	copy(sortedEdges, edges)
	sort.Slice(sortedEdges,
		func(i, j int) bool { return sortedEdges[i].cost < sortedEdges[j].cost })

	uf := NewUnionFind(nNodes + 1)

	for _, e := range sortedEdges {
		if uf.Volume() == 4 {
			return e.cost // result found
		} else {
			if uf.Connected(e.from, e.to) {
				// skip this edge
				PrintDebugLvl(2, "skip %d %d %d, k %d", e.from, e.to, e.cost, uf.Volume())
			} else {
				PrintDebugLvl(2, "fuse %d %d %d, k %d", e.from, e.to, e.cost, uf.Volume())
				uf.Union(e.from, e.to)
			}
		}
	}

	return 0
}

// usage:
//   go run clustering.go unionfind.go utils.go
func main() {
	EnableDebug()
	EnableDebug()

	fname := "clustering1.txt" // FIXME: command line
	k := 4                     // FIXME: command line

	fmt.Printf("Clustering: data \"%s\", k %d\n", fname, k)

	nNodes, edges := ReadEdges(fname)
	PrintDebugLvl(1, "data file \"%s\", nodes %d, edges %v", fname, nNodes, edges[:3])

	start := time.Now()
	res := MaxSpacingClustering(k, nNodes, edges)
	PrintExecTime(start, "MaxSpacingClustering()")

	fmt.Printf("%d\n", res)
}
