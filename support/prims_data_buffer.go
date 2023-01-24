package support

import (
	"encoding/binary"
	"math"
)

type Buffer []byte

func Data_buffer_prim__newBuffer(s, world any) Buffer {
	return Buffer(make([]byte, s.(int)))
}

func Data_buffer_prim__bufferSize(b any) int {
	return len(b.(Buffer))
}

func bufferWrite16(b Buffer, o int, data uint16) any {
	binary.LittleEndian.PutUint16(b[o:o+2], data)
	return nil
}

func bufferWrite32(b Buffer, o int, data uint32) any {
	binary.LittleEndian.PutUint32(b[o:o+4], data)
	return nil
}

func bufferWrite64(b Buffer, o int, data uint64) any {
	binary.LittleEndian.PutUint64(b[o:o+8], data)
	return nil
}

func bufferRead16(b Buffer, o int) uint16 {
	return binary.LittleEndian.Uint16(b[o : o+2])
}

func bufferRead32(b Buffer, o int) uint32 {
	return binary.LittleEndian.Uint32(b[o : o+4])
}

func bufferRead64(b Buffer, o int) uint64 {
	return binary.LittleEndian.Uint64(b[o : o+8])
}

func Data_buffer_prim__getBits8(b, o, world any) uint8 {
	return b.(Buffer)[o.(int)]
}

func Data_buffer_prim__setBits8(b, o, a, world any) any {
	b.(Buffer)[o.(int)] = a.(byte)
	return nil
}

func Data_buffer_prim__getBits16(b, o, world any) uint16 {
	return bufferRead16(b.(Buffer), o.(int))
}

func Data_buffer_prim__setBits16(b, o, a, world any) any {
	return bufferWrite16(b.(Buffer), o.(int), a.(uint16))
}

func Data_buffer_prim__getBits32(b, o, world any) uint32 {
	return bufferRead32(b.(Buffer), o.(int))
}

func Data_buffer_prim__setBits32(b, o, a, world any) any {
	return bufferWrite32(b.(Buffer), o.(int), a.(uint32))
}

func Data_buffer_prim__getBits64(b, o, world any) uint64 {
	return bufferRead64(b.(Buffer), o.(int))
}

func Data_buffer_prim__setBits64(b, o, a, world any) any {
	return bufferWrite64(b.(Buffer), o.(int), a.(uint64))
}

func Data_buffer_prim__getInt(b, o, world any) int {
	return int(bufferRead32(b.(Buffer), o.(int)))
}

func Data_buffer_prim__setInt(b, o, a, world any) any {
	return bufferWrite32(b.(Buffer), o.(int), uint32(a.(int)))
}

func Data_buffer_prim__getDouble(b, o, world any) float64 {
	return math.Float64frombits(bufferRead64(b.(Buffer), o.(int)))
}

func Data_buffer_prim__setDouble(b, o, a, world any) any {
	return bufferWrite64(b.(Buffer), o.(int), math.Float64bits(a.(float64)))
}

func Data_buffer_prim__getString(b, o, l, world any) string {
	offset := o.(int)
	length := l.(int)
	return string(append([]byte(nil), b.(Buffer)[offset:offset+length]...))
}

func Data_buffer_prim__setString(b, o, s, world any) any {
	copy(b.(Buffer)[o.(int):], []byte(s.(string)))
	return nil
}

func Data_buffer_stringByteLength(s any) int {
	return len([]byte(s.(string)))
}

func Data_buffer_prim__copyData(b1, o1, l, b2, o2, world any) any {
	length := l.(int)
	offset1, offset2 := o1.(int), o2.(int)
	copy(b2.(Buffer)[offset2:offset2+length], b1.(Buffer)[offset1:offset1+length])
	return nil
}
