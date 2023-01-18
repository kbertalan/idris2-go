package support

import (
	"bufio"
	"encoding/binary"
	"fmt"
	"io"
	"io/fs"
	"math"
	"os"
	"os/exec"
	"reflect"
	"runtime"
	"strings"
	"sync"
	"time"
)

type WorldType struct {
	stdin           *bufio.Reader
	stdout          *os.File
	stderr          *os.File
	lastFileInError *filePtr
	lastError       error
}

func NewWorld() *WorldType {
	return &WorldType{
		stdin:           bufio.NewReader(os.Stdin),
		stdout:          os.Stdout,
		stderr:          os.Stderr,
		lastFileInError: nil,
		lastError:       nil,
	}
}

func Idris2GoSlice[E any](v any) []E {
	var slice []E
	for value := v.(Value); value.Tag == 1; value = value.Args[1].(Value) {
		slice = append(slice, value.Args[0].(E))
	}
	return slice
}

func Go2IdrisSlice[E any](v []E) Value {
	value := Constructor(0)
	for i := len(v) - 1; i >= 0; i-- {
		value = Constructor(1, v[i], value)
	}
	return value
}

func Prelude_types_fastConcat(v any) string {
	strs := Idris2GoSlice[string](v)
	return strings.Join(strs, "")
}

func Prelude_io_prim__getString(v any) string {
	return *v.(*string)
}

func Prelude_io_prim__putChar(v, world any) any {
	_, err := world.(*WorldType).stdout.Write([]byte{v.(byte)})
	if err != nil {
		panic(err)
	}
	return nil
}

func Prelude_io_prim__getChar(w any) byte {
	world := w.(*WorldType)
	data := make([]byte, 1)
	_, err := world.stdin.Read(data)
	if err != nil {
		world.lastError = err
	}
	return data[0]
}

func Prelude_io_prim__getStr(world any) string {
	line, err := world.(*WorldType).stdin.ReadString('\n')
	if err != nil {
		panic(err)
	}
	return line[:len(line)-1] // trim new line
	// TODO check this on Windows
}

func Prelude_io_prim__putStr(v any, w any) any {
	world := w.(*WorldType)
	_, err := world.stdout.WriteString(v.(string))
	if err != nil {
		world.lastError = err
	}
	return nil
}

func Prelude_io_prim__fork(f any, w any) any {
	world := w.(*WorldType)
	fn := f.(func(v any) any)
	wg := &sync.WaitGroup{}
	wg.Add(1)
	go (func() {
		defer wg.Done()
		forkedWorld := world
		forkedWorld.lastError = nil
		forkedWorld.lastFileInError = nil
		fn(forkedWorld)
	})()

	return wg
}

func Prelude_io_prim__threadWait(v any, world any) any {
	wg := v.(*sync.WaitGroup)
	wg.Wait()
	return nil
}

func Prelude_types_fastPack(v any) string {
	return string(Idris2GoSlice[byte](v))
}

func Prelude_types_fastUnpack(v any) Value {
	return Go2IdrisSlice([]byte(v.(string)))
}

func Primio_prim__nullAnyPtr(v any) int {
	if v == nil || reflect.ValueOf(v).IsNil() {
		return 1
	}
	return 0
}

func System_clock_prim__clockTimeGcCpu(world any) any     { panic("not implemented") }
func System_clock_prim__clockTimeGcReal(world any) any    { panic("not implemented") }
func System_clock_prim__clockTimeMonotonic(world any) any { panic("not implemented") }
func System_clock_prim__clockTimeProcess(world any) any   { panic("not implemented") }
func System_clock_prim__clockTimeThread(world any) any    { panic("not implemented") }

func System_clock_prim__clockTimeUtc(world any) time.Time {
	return time.Now().UTC()
}

func System_clock_prim__osClockNanosecond(v, world any) uint64 {
	return uint64(v.(time.Time).Nanosecond())
}

func System_clock_prim__osClockSecond(v, world any) uint64 {
	return uint64(v.(time.Time).Second())
}

func System_clock_prim__osClockValid(v, world any) any { panic("not implemented") }

func System_concurrency_prim__channelGet(a, ch, world any) any {
	result := <-ch.(chan any)
	return result
}

func System_concurrency_prim__channelPut(a, ch, v, world any) any {
	ch.(chan any) <- v
	return nil
}

func System_concurrency_prim__makeChannel(a, world any) chan any {
	return make(chan any)
}

func System_errno_prim__strerror(errno, world any) any {
	return fmt.Sprintf("Error number: %d", errno.(int))
}

func System_ffi_prim__free(v, world any) any {
	return nil
}

func System_file_error_prim__fileErrno(w any) int {
	world := w.(*WorldType)
	switch world.lastFileInError.lastError {
	case io.ErrShortBuffer:
		return 0 // FileReadError
	case io.EOF, io.ErrUnexpectedEOF, os.ErrClosed, io.ErrClosedPipe, io.ErrShortWrite:
		return 1 // FileWriteError
	case os.ErrNotExist:
		return 2 // FileNotFound
	case os.ErrPermission:
		return 3 // PermissionDenied
	case os.ErrExist:
		return 4 // FileExists
	}

	switch world.lastFileInError.lastError.(type) {
	case *fs.PathError:
		return 2 // FileNotFound
	}

	return 5 // GenericFileError
}

type filePtr struct {
	file      *os.File
	reader    *bufio.Reader
	writer    *bufio.Writer
	eof       bool
	lastError error
}

func System_file_handle_prim__close(f, w any) any {
	world := w.(*WorldType)
	filePtr := f.(*filePtr)
	err := filePtr.file.Close()
	if err != nil {
		filePtr.lastError = err
		world.lastFileInError = filePtr
		return nil
	}
	world.lastFileInError = nil
	return nil
}

func System_file_handle_prim__open(f, m, w any) *filePtr {
	world := w.(*WorldType)
	mode := 0
	var reader, writer bool
	switch m.(string) {
	case "r", "rb":
		mode |= os.O_RDONLY
		reader = true
	case "w", "wb":
		mode |= os.O_WRONLY | os.O_TRUNC | os.O_CREATE
		writer = true
	case "a", "ab":
		mode |= os.O_WRONLY | os.O_APPEND
		writer = true
	case "r+", "rb+":
		mode |= os.O_RDWR
		reader = true
		writer = true
	case "w+", "wb+":
		mode |= os.O_RDWR | os.O_TRUNC | os.O_CREATE
		reader = true
		writer = true
	case "a+", "ab+":
		mode |= os.O_RDWR | os.O_APPEND
		reader = true
		writer = true
	}
	file, err := os.OpenFile(f.(string), mode, 0644)
	if err != nil {
		filePtr := filePtr{
			file:      file,
			lastError: err,
		}
		world.lastFileInError = &filePtr
		return nil
	}
	world.lastFileInError = nil
	ptr := filePtr{
		file: file,
	}
	if reader {
		ptr.reader = bufio.NewReader(file)
	}
	if writer {
		ptr.writer = bufio.NewWriter(file)
	}
	return &ptr
}

func System_file_readwrite_prim__eof(f, w any) any {
	file := f.(*filePtr)
	if file.eof {
		return 1
	}
	return 0
}

func System_file_readwrite_prim__readLine(f, w any) *string {
	world := w.(*WorldType)
	file := f.(*filePtr)
	line, err := file.reader.ReadString('\n')
	if err != nil {
		if err != io.EOF {
			file.lastError = err
			world.lastFileInError = file
			return nil
		}
		file.eof = true
	}
	world.lastFileInError = nil
	return &line
}

func System_file_readwrite_prim__seekLine(f, world any) any {
	result := System_file_readwrite_prim__readLine(f, world)
	if result == nil {
		return 1
	}
	return 0
}

func System_file_readwrite_prim__writeLine(f, l, w any) any {
	world := w.(*WorldType)
	file := f.(*filePtr)
	_, err := file.writer.WriteString(l.(string))
	if err != nil {
		file.lastError = err
		world.lastFileInError = file
		return 0
	}
	err = file.writer.Flush()
	if err != nil {
		file.lastError = err
		world.lastFileInError = file
		return 0
	}
	world.lastFileInError = nil
	return 1
}

func Main_system_info_prim__os() any {
	return runtime.GOOS
}

func System_prim__exit(code, world any) any {
	os.Exit(code.(int))
	return nil
}

func System_prim__getArg(n, world any) any {
	return os.Args[n.(int)]
}

func System_prim__getArgCount(world any) any {
	return len(os.Args)
}

func System_prim__getEnv(v, world any) *string {
	value, ok := os.LookupEnv(v.(string))
	if !ok {
		return nil
	}
	return &value
}

func System_prim__setEnv(n, v, o, w any) int {
	world := w.(*WorldType)
	name := n.(string)
	value := v.(string)
	overwrite := o.(int) != 0
	_, exists := os.LookupEnv(name)
	if exists && !overwrite {
		return 0
	}
	err := os.Setenv(name, value)
	if err != nil {
		world.lastError = err
		return 1
	}
	return 0
}

func System_prim__system(v, w any) any {
	world := w.(*WorldType)
	cmd := exec.Command("sh", "-c", v.(string))

	cmd.Stdout = world.stdout
	cmd.Stderr = world.stderr
	err := cmd.Run()
	if err != nil {
		if exitErr, ok := err.(*exec.ExitError); ok {
			return exitErr.ProcessState.ExitCode()
		}
		return -1
	}
	return 0
}

func Main_data_ioref_prim__newIORef(tya, a, world any) *any {
	return &a
}

func Main_data_ioref_prim__readIORef(tya, ref, world any) any {
	return *ref.(*any)
}

func Main_data_ioref_prim__writeIORef(tya, ref, a, world any) any {
	*ref.(*any) = a
	return nil
}

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
	copy([]byte(s.(string)), b.(Buffer)[o.(int):])
	return nil
}

func Data_buffer_stringByteLength(s any) int {
	return len([]byte(s.(string)))
}

func Data_buffer_prim__copyData(b1, o1, l, b2, o2, world any) any {
	length := l.(int)
	offset1, offset2 := o1.(int), o2.(int)
	copy(b1.(Buffer)[offset1:offset1+length], b2.(Buffer)[offset2:offset2+length])
	return nil
}

func Main_data_ioarray_prims_prim__newArray(_, size, a, world any) []any {
	arr := make([]any, size.(int))
	for i := range arr {
		arr[i] = a
	}
	return arr
}

func Main_data_ioarray_prims_prim__arrayGet(_, arr, i, world any) any {
	return arr.([]any)[i.(int)]
}

func Main_data_ioarray_prims_prim__arraySet(_, arr, i, a, world any) any {
	arr.([]any)[i.(int)] = a
	return nil
}
