package support

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"os/exec"
	"reflect"
	"runtime"
	"strings"
	"sync"
	"time"
)

var (
	stdin  = bufio.NewReader(os.Stdin)
	stdout = bufio.NewWriter(os.Stdout)
)

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

func Prelude_io_prim__putChar(v any) any {
	_, err := stdout.WriteRune(v.(rune))
	if err != nil {
		panic(err)
	}
	flushStdout()
	return nil
}

func Prelude_io_prim__getChar() rune {
	r, _, err := stdin.ReadRune()
	if err != nil {
		panic(err)
	}
	return r
}

func Prelude_io_prim__getStr(world any) string {
	line, err := stdin.ReadString('\n')
	if err != nil {
		panic(err)
	}
	return line
}

func Prelude_io_prim__putStr(v any, world any) any {
	_, err := stdout.WriteString(v.(string))
	if err != nil {
		panic(err)
	}
	flushStdout()
	return nil
}

func Prelude_io_prim__fork(f any, world any) any {
	fn := f.(func(v any) any)
	wg := &sync.WaitGroup{}
	wg.Add(1)
	go (func() {
		defer wg.Done()
		fn(world)
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
	return <-ch.(chan any)
}

func System_concurrency_prim__channelPut(a, ch, v, world any) any {
	ch.(chan any) <- v
	return nil
}

func System_concurrency_prim__makeChannel(a, world any) chan any {
	return make(chan any, 1)
}

func System_errno_prim__strerror(errno, world any) any {
	return fmt.Sprintf("Error number: %d", errno.(int))
}

func System_ffi_prim__free(v, world any) any {
	return nil
}

var lastFileError error = nil

func System_file_error_prim__fileErrno(world any) int {
	switch lastFileError {
	// TODO add read/write errors
	// case ??: return 0 // FileReadError
	// case ??: return 1 // FileWriteError
	case os.ErrNotExist:
		return 2 // FileNotFound
	case os.ErrPermission:
		return 3 // PermissionDenied
	case os.ErrExist:
		return 4 // FileExists
	default:
		return 5
	}
}

type filePtr struct {
	file   *os.File
	reader *bufio.Reader
	writer *bufio.Writer
}

type bufferedReader interface {
}

func System_file_handle_prim__close(f, world any) any {
	err := f.(*filePtr).file.Close()
	if err != nil {
		lastFileError = err
		return nil
	}
	return nil
}

func System_file_handle_prim__open(f, m, world any) *filePtr {
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
		lastFileError = err
		return nil
	}
	lastFileError = nil
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

func System_file_readwrite_prim__eof(f, world any) any {
	file := f.(*filePtr)
	_, err := file.reader.Peek(1)
	if err != nil {
		if err == io.EOF || err == io.ErrUnexpectedEOF {
			return 1
		}
	}
	return 0
}

func System_file_readwrite_prim__readLine(f, world any) *string {
	file := f.(*filePtr)
	line, err := file.reader.ReadString('\n')
	if err != nil {
		lastFileError = err
		return nil
	}
	lastFileError = nil
	return &line
}

func System_file_readwrite_prim__seekLine(f, world any) any {
	result := System_file_readwrite_prim__readLine(f, world)
	if result == nil {
		return 1
	}
	return 0
}

func System_file_readwrite_prim__writeLine(f, l, world any) any {
	file := f.(*filePtr)
	_, err := file.writer.WriteString(l.(string))
	if err != nil {
		lastFileError = err
		return 0
	}
	lastFileError = nil
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

func System_prim__system(v, world any) any {
	cmd := exec.Command("sh", "-c", v.(string))
	err := cmd.Run()
	if err != nil {
		if exitErr, ok := err.(*exec.ExitError); ok {
			return exitErr.ProcessState.ExitCode()
		}
		return -1
	}
	return 0
}

func flushStdout() {
	if err := stdout.Flush(); err != nil {
		panic(err)
	}
}
