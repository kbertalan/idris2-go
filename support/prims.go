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
	"syscall"
	"time"
	"unsafe"
)

var bufferedStdin = bufio.NewReader(os.Stdin)

type WorldType struct {
	lastFileInError *filePtr
	lastError       error
	threadStartTime time.Time
}

func NewWorld() *WorldType {
	return &WorldType{
		lastFileInError: nil,
		lastError:       nil,
		threadStartTime: time.Now(),
	}
}

func (w *WorldType) Fork() *WorldType {
	world := *w
	world.lastError = nil
	world.lastFileInError = nil
	world.threadStartTime = time.Now()
	return &world
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
	_, err := os.Stdout.Write([]byte{v.(byte)})
	if err != nil {
		panic(err)
	}
	return nil
}

func Prelude_io_prim__getChar(w any) byte {
	world := w.(*WorldType)
	data := make([]byte, 1)
	_, err := bufferedStdin.Read(data)
	if err != nil {
		world.lastError = err
	}
	return data[0]
}

func Prelude_io_prim__getStr(world any) string {
	line, err := bufferedStdin.ReadString('\n')
	if err != nil {
		panic(err)
	}
	return line[:len(line)-1] // trim new line
	// TODO check this on Windows
}

func Prelude_io_prim__putStr(v any, w any) any {
	world := w.(*WorldType)
	_, err := os.Stdout.WriteString(v.(string))
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
		fn(world.Fork())
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

func System_clock_prim__clockTimeGcCpu(world any) any  { panic("not implemented") }
func System_clock_prim__clockTimeGcReal(world any) any { panic("not implemented") }

func System_clock_prim__clockTimeMonotonic(world any) time.Time {
	// go's time is monotonic between 1885 and 2157 years
	return time.Now()
}

var processStartTime = time.Now()

func System_clock_prim__clockTimeProcess(world any) time.Duration {
	return time.Since(processStartTime)
}

func System_clock_prim__clockTimeThread(w any) any {
	world := w.(*WorldType)
	return time.Since(world.threadStartTime)
}

func System_clock_prim__clockTimeUtc(world any) time.Time {
	return time.Now().UTC()
}

func System_clock_prim__osClockNanosecond(v, world any) uint64 {
	switch t := v.(type) {
	case time.Time:
		return uint64(t.Nanosecond())
	case time.Duration:
		return uint64(t % 1e9)
	}
	panic(fmt.Sprintf("unknown time type %T", v))
}

func System_clock_prim__osClockSecond(v, world any) uint64 {
	switch t := v.(type) {
	case time.Time:
		return uint64(t.Unix())
	case time.Duration:
		return uint64(t / 1e9)
	}
	panic(fmt.Sprintf("unknown time type %T", v))
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

func System_errno_prim__getErrno(w any) any {
	world := w.(*WorldType)
	if world.lastError != nil {
		return 1 // TODO find out what errno-s can be extracted from errors
	}
	return -1
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
	createReader := false
	switch m.(string) {
	case "r", "rb":
		mode |= os.O_RDONLY
		createReader = true
	case "w", "wb":
		mode |= os.O_WRONLY | os.O_TRUNC | os.O_CREATE
	case "a", "ab":
		mode |= os.O_WRONLY | os.O_APPEND
	case "r+", "rb+":
		mode |= os.O_RDWR
		createReader = true
	case "w+", "wb+":
		mode |= os.O_RDWR | os.O_TRUNC | os.O_CREATE
		createReader = true
	case "a+", "ab+":
		mode |= os.O_RDWR | os.O_APPEND
		createReader = true
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
	if createReader {
		ptr.reader = bufio.NewReader(file)
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
	ptr := f.(*filePtr)
	line, err := ptr.reader.ReadString('\n')
	if err != nil {
		if err != io.EOF {
			ptr.lastError = err
			world.lastFileInError = ptr
			return nil
		}
		ptr.eof = true
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
	ptr := f.(*filePtr)
	_, err := ptr.file.WriteString(l.(string))
	if err != nil {
		ptr.lastError = err
		world.lastFileInError = ptr
		return 0
	}
	world.lastFileInError = nil
	return 1
}

func Main_system_info_prim__os() any {
	return runtime.GOOS
}

func Main_system_info_prim__codegen() string {
	return "go"
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

func System_prim__getEnvPair(n, w any) *string {
	i := n.(int)
	env := os.Environ()
	if i >= 0 && i < len(env) {
		pair := env[i]
		return &pair
	}
	return nil
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

func System_prim__unsetEnv(n, w any) int {
	if err := os.Unsetenv(n.(string)); err != nil {
		world := w.(*WorldType)
		world.lastError = err
		return 1
	}
	return 0
}

func System_prim__system(v, w any) any {
	cmd := exec.Command("sh", "-c", v.(string))

	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
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

func Libraries_data_string_iterator_fromString(s any) int {
	return 0
}

func Libraries_data_string_iterator_uncons(s, i any) Value {
	str := []byte(s.(string))
	index := i.(int)
	if index < len(str) {
		return Constructor(1, str[index], index+1)
	}
	return Constructor(0)
}

func Libraries_system_file_buffer_prim__readBufferData(f, b, o, m, w any) int {
	offset := o.(int)
	buffer := b.(Buffer)[offset : offset+m.(int)]
	ptr := f.(*filePtr)
	n, err := ptr.reader.Read(buffer)
	if err != nil {
		ptr.lastError = err
		world := w.(*WorldType)
		world.lastFileInError = ptr
		return -1
	}

	return n
}

func Libraries_system_file_buffer_prim__writeBufferData(f, b, o, s, w any) int {
	offset := o.(int)
	buffer := b.(Buffer)[offset : offset+s.(int)]
	ptr := f.(*filePtr)
	n, err := ptr.file.Write(buffer)
	if err != nil {
		ptr.lastError = err
		world := w.(*WorldType)
		world.lastFileInError = ptr
		return -1
	}
	return n
}

func Idris_idemode_repl_prim__idrnet_fdopen(a, b, w any) any { panic("not implemented") }
func Libraries_utils_scheme_prim__evalOkay(w any) any        { panic("not implemented") }
func Libraries_utils_scheme_prim__evalResult(w any) any      { panic("not implemented") }
func Libraries_utils_scheme_prim__evalScheme(w any) any      { panic("not implemented") }
func Libraries_utils_scheme_prim_isBox(w any) any            { panic("not implemented") }
func Libraries_utils_scheme_prim_isChar(w any) any           { panic("not implemented") }
func Libraries_utils_scheme_prim_isFloat(w any) any          { panic("not implemented") }
func Libraries_utils_scheme_prim_isInteger(w any) any        { panic("not implemented") }
func Libraries_utils_scheme_prim_isPair(w any) any           { panic("not implemented") }
func Libraries_utils_scheme_prim_isProcedure(w any) any      { panic("not implemented") }
func Libraries_utils_scheme_prim_isString(w any) any         { panic("not implemented") }
func Libraries_utils_scheme_prim_isSymbol(w any) any         { panic("not implemented") }
func Libraries_utils_scheme_prim_isVector(w any) any         { panic("not implemented") }
func Libraries_utils_scheme_unsafeApply(a, w any) any        { panic("not implemented") }
func Libraries_utils_scheme_unsafeForce(w any) any           { panic("not implemented") }
func Libraries_utils_scheme_unsafeFst(w any) any             { panic("not implemented") }
func Libraries_utils_scheme_unsafeGetChar(w any) any         { panic("not implemented") }
func Libraries_utils_scheme_unsafeGetFloat(w any) any        { panic("not implemented") }
func Libraries_utils_scheme_unsafeGetInteger(w any) any      { panic("not implemented") }
func Libraries_utils_scheme_unsafeGetString(w any) any       { panic("not implemented") }
func Libraries_utils_scheme_unsafeReadSymbol(w any) any      { panic("not implemented") }
func Libraries_utils_scheme_unsafeSnd(w any) any             { panic("not implemented") }
func Libraries_utils_scheme_unsafeUnbox(w any) any           { panic("not implemented") }
func Libraries_utils_scheme_unsafeVectorLength(w any) any    { panic("not implemented") }
func Libraries_utils_scheme_unsafeVectorRef(a, w any) any    { panic("not implemented") }
func Libraries_utils_scheme_unsafeVectorToList(w any) any    { panic("not implemented") }

type winsize struct {
	Row    uint16
	Col    uint16
	Xpixel uint16
	Ypixel uint16
}

func Libraries_utils_term_prim__getTermCols(w any) any {
	ws := &winsize{}
	retCode, _, errno := syscall.Syscall(syscall.SYS_IOCTL,
		uintptr(syscall.Stdin),
		uintptr(syscall.TIOCGWINSZ),
		uintptr(unsafe.Pointer(ws)))

	if int(retCode) == -1 {
		panic(errno)
	}
	return uint(ws.Col)
}

func Libraries_utils_term_prim__setupTerm(w any) any {
	return nil
}

func Network_ffi_prim__idrnet_accept(a, b, w any) any        { panic("not implemented") }
func Network_ffi_prim__idrnet_bind(a, b, c, d, e, w any) any { panic("not implemented") }
func Network_ffi_prim__idrnet_create_sockaddr(w any) any     { panic("not implemented") }
func Network_ffi_prim__idrnet_listen(a, b, w any) any        { panic("not implemented") }
func Network_ffi_prim__idrnet_sockaddr_family(a, w any) any  { panic("not implemented") }
func Network_ffi_prim__idrnet_sockaddr_ipv4(a, w any) any    { panic("not implemented") }
func Network_ffi_prim__idrnet_sockaddr_port(a, w any) any    { panic("not implemented") }
func Network_ffi_prim__idrnet_sockaddr_unix(a, w any) any    { panic("not implemented") }
func Network_ffi_prim__idrnet_socket(a, b, c, w any) any     { panic("not implemented") }
func Network_socket_data_prim__idrnet_af_inet(w any) any     { panic("not implemented") }
func Network_socket_data_prim__idrnet_af_inet6(w any) any    { panic("not implemented") }
func Network_socket_data_prim__idrnet_af_unix(w any) any     { panic("not implemented") }
func Network_socket_data_prim__idrnet_af_unspec(w any) any   { panic("not implemented") }
func Network_socket_data_prim__idrnet_errno(w any) any       { panic("not implemented") }

func Main_prelude_uninhabited_prim__void(a, w any) any {
	panic("executed void")
}

func System_directory_prim__changeDir(d, w any) int {
	err := os.Chdir(d.(string))
	if err != nil {
		w.(*WorldType).lastError = err
		return 1
	}
	return 0
}

type dirPtr struct {
	entries []os.DirEntry
	current int
}

func System_directory_prim__closeDir(d, w any) any {
	ptr := d.(*dirPtr)
	ptr.entries = nil
	ptr.current = 0
	return nil
}

func System_directory_prim__createDir(d, w any) int {
	err := os.Mkdir(d.(string), 0750)
	if err != nil {
		w.(*WorldType).lastError = err
		return 1
	}
	return 0
}

func System_directory_prim__currentDir(w any) *string {
	cwd, err := os.Getwd()
	if err != nil {
		w.(*WorldType).lastError = err
		return nil
	}
	return &cwd
}

func System_directory_prim__dirEntry(d, w any) *string {
	w.(*WorldType).lastError = nil
	ptr := d.(*dirPtr)
	if ptr.current < len(ptr.entries) {
		name := ptr.entries[ptr.current].Name()
		ptr.current++
		return &name
	}
	return nil
}

func System_directory_prim__openDir(d, w any) *dirPtr {
	entries, err := os.ReadDir(d.(string))
	if err != nil {
		w.(*WorldType).lastError = err
		return nil
	}
	return &dirPtr{
		entries: entries,
		current: 0,
	}
}

func System_directory_prim__removeDir(d, w any) any {
	err := os.Remove(d.(string))
	w.(*WorldType).lastError = err
	return nil
}

func System_file_buffer_prim__readBufferData(f, b, o, m, w any) any {
	return Libraries_system_file_buffer_prim__readBufferData(f, b, o, m, w)
}

func System_file_buffer_prim__writeBufferData(f, b, o, s, w any) any {
	return Libraries_system_file_buffer_prim__writeBufferData(f, b, o, s, w)
}

func System_file_error_prim__error(f, w any) int {
	if f.(*filePtr).lastError != nil {
		return 1
	}
	return 0
}

func System_file_meta_prim__fileModifiedTime(f, w any) int {
	fname := f.(string)
	info, _ := os.Stat(fname)
	return int(info.ModTime().Unix())
}

func System_file_meta_prim__fileSize(f, w any) any {
	fname := f.(string)
	info, _ := os.Stat(fname)
	return int(info.Size())
}

func System_file_permissions_prim__chmod(f, p, w any) int {
	world := w.(*WorldType)
	err := os.Chmod(f.(string), os.FileMode(p.(int)))
	if err != nil {
		world.lastFileInError = &filePtr{
			lastError: err,
		}
		return 1
	}
	world.lastFileInError = nil
	return 0
}

func System_file_process_prim__flush(a, w any) any      { panic("not implemented") }
func System_file_process_prim__pclose(a, w any) any     { panic("not implemented") }
func System_file_process_prim__popen(a, b, w any) any   { panic("not implemented") }
func System_file_readwrite_prim__readChar(a, w any) any { panic("not implemented") }

func System_file_readwrite_prim__removeFile(f, w any) int {
	world := w.(*WorldType)
	fname := f.(string)
	err := os.Remove(fname)
	if err != nil {
		world.lastFileInError = &filePtr{
			lastError: err,
		}
		return 1
	}
	world.lastFileInError = nil
	return 0
}

func System_file_virtual_prim__stdin() *filePtr {
	return &filePtr{
		file:   os.Stdin,
		reader: bufferedStdin,
	}
}

func System_file_virtual_prim__stdout() *filePtr {
	return &filePtr{
		file: os.Stdout,
	}
}

func System_file_virtual_prim__stderr() *filePtr {
	return &filePtr{
		file: os.Stderr,
	}
}
