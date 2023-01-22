package support

import (
	"os"
	"reflect"
	"strings"
	"sync"
)

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
	_, err := stdin.reader.Read(data)
	if err != nil {
		world.lastError = err
	}
	return data[0]
}

func Prelude_io_prim__getStr(world any) string {
	line, err := stdin.reader.ReadString('\n')
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
