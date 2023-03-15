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
	_, err := os.Stdout.WriteString(string(v.(rune)))
	if err != nil {
		panic(err)
	}
	return nil
}

func Prelude_io_prim__getChar(w any) rune {
	world := GetWorld(w)
	r, _, err := stdin.reader.ReadRune()
	world.SetLastError(err)
	return r
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
	world := GetWorld(w)
	_, err := os.Stdout.WriteString(v.(string))
	world.SetLastError(err)
	return nil
}

func Prelude_io_prim__fork(f any, w any) any {
	world := GetWorld(w)
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
	return string(Idris2GoSlice[rune](v))
}

func Prelude_types_fastUnpack(v any) Value {
	return Go2IdrisSlice([]rune(v.(string)))
}

func Primio_prim__nullAnyPtr(v any) int {
	if v == nil || reflect.ValueOf(v).IsNil() {
		return 1
	}
	return 0
}
