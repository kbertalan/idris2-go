package support

import "fmt"

type Vector interface {
	Len() int
	Get(i int) any
	Append(i ...any) Vector
	Last() any
	Init() Vector
	Iter() Iterator
	ReverseIter() Iterator
}

type Iterator interface {
	HasNext() bool
	Next() any
}

type vector struct {
	// 2^bits items per node
	bits uint
	// how many bits can cover all indices, always divisable with bits
	cover uint
	// actual length of the vector
	len uint
	// root node items
	data []any
	// actual tail, an optimisation to skip tail lookup
	tail []any
}

func NewVector(bits uint) Vector {
	return vector{
		bits:  bits,
		cover: bits,
		len:   0,
		data:  nil,
		tail:  nil,
	}
}

func (v vector) width() uint {
	return 1 << v.bits
}

func (v vector) mask() uint {
	return (1 << v.bits) - 1
}

func (v vector) sliceFor(i uint) []any {
	if i >= v.len {
		panic(fmt.Sprintf("index %d is out of bounds, length is %d", i, v.len))
	}
	tailStart := ((v.len - 1) >> v.bits) << v.bits
	if i >= tailStart {
		return v.tail
	}

	mask := v.mask()
	var current any = v.data
	for cover := v.cover; cover >= v.bits; cover -= v.bits {
		index := (i >> cover) & mask
		current = current.([]any)[index]
	}
	return current.([]any)
}

func (v vector) Append(elems ...any) Vector {
	width := v.width()

	current := v
	currentLength := v.len
	desiredLength := currentLength + uint(len(elems))

	for currentLength < desiredLength {
		if len(current.tail) == int(width) {
			current = current.pushTailToLeaf()
		}

		batchLen := minInt(int(width)-len(current.tail), len(elems))

		newTail := make([]any, 0, len(current.tail)+batchLen)
		newTail = append(newTail, current.tail...)
		newTail = append(newTail, elems[0:batchLen]...)

		currentLength += uint(batchLen)
		current = vector{
			bits:  current.bits,
			cover: current.cover,
			len:   currentLength,
			data:  current.data,
			tail:  newTail,
		}
		elems = elems[batchLen:]
	}

	return current
}

func (v vector) pushTailToLeaf() vector {
	var newData []any
	newCover := v.cover
	width := v.width()
	leaf := v.tail

	if ((v.len - 1) >> v.cover) >= width {
		newNode := vectorNewPathForLeaf(v.bits, v.cover, leaf)
		newData = []any{v.data, newNode}
		newCover = v.cover + v.bits
	} else {
		newData = vectorPushNodeToLeaf(v.bits, v.len, v.mask(), v.cover, v.data, leaf)
	}

	return vector{
		bits:  v.bits,
		cover: newCover,
		len:   v.len,
		data:  newData,
		tail:  nil,
	}
}

func (v vector) appendAt(at uint, items ...any) vector {
	truncated := v.dropFrom(at)
	return truncated.Append(items...).(vector)
}

func (v vector) dropFrom(from uint) vector {
	if from == 0 {
		return NewVector(v.bits).(vector)
	}
	if from >= v.len {
		return v
	}
	tailStart := ((v.len - 1) >> v.bits) << v.bits
	if from > tailStart {
		return vector{
			bits:  v.bits,
			cover: v.cover,
			len:   from,
			data:  v.data,
			tail:  v.tail[:from-tailStart],
		}
	}

	mask := v.mask()
	cover := v.cover
	newData := v.data
	for ; cover > v.bits; cover -= v.bits {
		index := ((from - 1) >> cover) & mask
		if index > 0 {
			data, tail := vectorDropFrom(v.bits, mask, cover-v.bits, from, newData[index].([]any))
			if len(data) > 0 {
				merged := make([]any, 0, index+1)
				merged = append(merged, newData[:index]...)
				merged = append(merged, data)
				return vector{
					bits:  v.bits,
					cover: cover,
					len:   from,
					data:  merged,
					tail:  tail,
				}
			}
			if index > 1 {
				return vector{
					bits:  v.bits,
					cover: cover - v.bits,
					len:   from,
					data:  newData[:index],
					tail:  tail,
				}
			}
			return vector{
				bits:  v.bits,
				cover: cover - v.bits,
				len:   from,
				data:  newData[0].([]any),
				tail:  tail,
			}
		}
		newData = newData[index].([]any)
	}

	innerNodeIndex := ((from - 1) >> cover) & mask
	index := (from - 1) & mask
	return vector{
		bits:  v.bits,
		cover: v.bits,
		len:   from,
		data:  newData[:innerNodeIndex],
		tail:  newData[innerNodeIndex].([]any)[:index+1],
	}
}

func (v vector) Len() int {
	return int(v.len)
}

func (v vector) Get(i int) any {
	if i < 0 || i > int(v.len) {
		panic(fmt.Sprintf("index out of range %d, length is %d", i, v.len))
	}

	ix := uint(i)

	mask := v.mask()
	tailStart := ((v.len - 1) >> v.bits) << v.bits
	if v.len <= v.width() || ix >= tailStart {
		return v.tail[ix-tailStart]
	}

	current := v.data
	for cover := v.cover; cover > 0; cover -= v.bits {
		index := (ix >> cover) & mask
		current = current[index].([]any)
	}
	return current[ix&mask]
}

func (v vector) Last() any {
	if v.len == 0 {
		panic("vector is empty, cannot use Last")
	}

	if len(v.tail) > 0 {
		return v.tail[len(v.tail)-1]
	}

	return v.Get(int(v.len) - 1)
}

func (v vector) Init() Vector {
	if v.len == 0 {
		panic("vector is empty, cannot use Init")
	}
	return newVectorView(v, 0, v.len-1)
}

func (v vector) Iter() Iterator {
	return newVectorIter(0, v.len, v)
}

func (v vector) ReverseIter() Iterator {
	return newVectorReverseIter(0, v.len, v)
}

type vectorView struct {
	vec      vector
	from, to uint
}

func newVectorView(v vector, from, to uint) vectorView {
	if from >= v.len {
		panic(fmt.Sprintf("vector view cannot start (%d) out of range of the vector (length: %d)", from, v.len))
	}
	if to >= v.len {
		panic(fmt.Sprintf("vector view cannot end (%d) out of range of the vector (length: %d)", to, v.len))
	}
	if from > to {
		panic("vector view cannot start at index greater than the index of end")
	}

	return vectorView{
		vec:  v,
		from: from,
		to:   to,
	}
}

func (vv vectorView) Append(i ...any) Vector {
	return vectorView{
		vec:  vv.vec.appendAt(vv.to, i...),
		from: vv.from,
		to:   vv.to + uint(len(i)),
	}
}

func (vv vectorView) Get(i int) any {
	if i < 0 || i >= vv.Len() {
		panic(fmt.Sprintf("index out of range %d, length is %d", i, vv.Len()))
	}
	ix := uint(i) - vv.from
	return vv.vec.Get(int(ix))
}

func (vv vectorView) Init() Vector {
	if vv.from >= vv.to {
		panic("vector view is empty, cannot use Init")
	}
	return vectorView{
		vec:  vv.vec,
		from: vv.from,
		to:   vv.to - 1,
	}
}

func (vv vectorView) Last() any {
	if vv.from >= vv.to {
		panic("vector view is empty, cannot use Last")
	}
	return vv.vec.Get(int(vv.to) - 1)
}

func (vv vectorView) Len() int {
	return int(vv.to) - int(vv.from)
}

func (vv vectorView) Iter() Iterator {
	return newVectorIter(vv.from, vv.to, vv.vec)
}

func (vv vectorView) ReverseIter() Iterator {
	return newVectorReverseIter(vv.from, vv.to, vv.vec)
}

type vectorIter struct {
	till uint
	vec  vector

	// mutable
	index, idx uint
	data       []any
}

func newVectorIter(from, to uint, vec vector) *vectorIter {
	data := vec.sliceFor(from)
	idx := from & vec.mask()

	return &vectorIter{
		till: to,
		vec:  vec,

		index: from,
		idx:   idx,
		data:  data,
	}
}

func (s *vectorIter) HasNext() bool {
	return s.index < s.till
}

func (s *vectorIter) Next() any {
	result := s.data[s.idx]

	s.index++
	s.idx++

	if !s.HasNext() {
		s.data = nil
		return result
	}

	if uint(s.idx) >= s.vec.width() {
		s.data = s.vec.sliceFor(s.index)
		s.idx = 0
	}

	return result
}

type vectorReverseIter struct {
	till uint
	vec  vector

	// mutable
	index, idx int
	data       []any
}

func newVectorReverseIter(from, to uint, vec vector) *vectorReverseIter {
	index := int(to) - 1
	var data []any
	var idx int
	if index >= 0 {
		data = vec.sliceFor(uint(index))
		idx = index & int(vec.mask())
	}

	return &vectorReverseIter{
		till: from,
		vec:  vec,

		index: index,
		idx:   idx,
		data:  data,
	}
}

func (s *vectorReverseIter) HasNext() bool {
	return s.index >= int(s.till)
}

func (s *vectorReverseIter) Next() any {
	result := s.data[s.idx]

	s.index--
	s.idx--

	if !s.HasNext() {
		s.data = nil
		return result
	}

	if s.idx < 0 {
		s.data = s.vec.sliceFor(uint(s.index))
		s.idx = int(s.vec.width()) - 1
	}

	return result
}

func vectorDropFrom(bits, mask, cover, from uint, node []any) ([]any, []any) {
	newData := node
	for ; cover > bits; cover -= bits {
		index := ((from - 1) >> cover) & mask
		if index != 0 {
			data, tail := vectorDropFrom(bits, mask, cover-bits, from, newData[index].([]any))
			if len(data) > 0 {
				merged := make([]any, 0, index+1)
				merged = append(merged, newData[:index]...)
				merged = append(merged, data)
				return merged, tail
			}

			return newData[:index], tail
		}
		newData = newData[index].([]any)
	}

	innerNodeIndex := ((from - 1) >> cover) & mask
	index := (from - 1) & mask
	data := newData[:innerNodeIndex]
	tail := newData[innerNodeIndex].([]any)[:index+1]
	return data, tail
}

func vectorPushNodeToLeaf(bits, length, mask, cover uint, parent, node []any) []any {
	index := ((length - 1) >> cover) & mask
	newNode := make([]any, index+1)
	copy(newNode, parent)
	var toInsert []any

	switch {
	case cover == bits:
		toInsert = node
	case index < uint(len(parent)):
		toInsert = vectorPushNodeToLeaf(bits, length, mask, cover-bits, parent[index].([]any), node)
	default:
		toInsert = vectorNewPathForLeaf(bits, cover-bits, node)
	}

	newNode[index] = toInsert
	return newNode
}

func vectorNewPathForLeaf(bits, cover uint, node []any) []any {
	for ; cover > 0; cover -= bits {
		node = []any{node}
	}
	return node
}

func minInt(a, b int) int {
	if a > b {
		return b
	}
	return a
}
