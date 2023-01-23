package support

import (
	"syscall"
	"unsafe"
)

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
	return System_file_buffer_prim__readBufferData(f, b, o, m, w)
}

func Libraries_system_file_buffer_prim__writeBufferData(f, b, o, s, w any) int {
	return System_file_buffer_prim__writeBufferData(f, b, o, s, w)
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

func Libraries_utils_term_prim__getTermCols(w any) int {
	ws := &winsize{}
	retCode, _, errno := syscall.Syscall(syscall.SYS_IOCTL,
		uintptr(syscall.Stdin),
		uintptr(syscall.TIOCGWINSZ),
		uintptr(unsafe.Pointer(ws)))

	if int(retCode) == -1 {
		panic(errno)
	}
	return int(ws.Col)
}

func Libraries_utils_term_prim__getTermLines(w any) int {
	ws := &winsize{}
	retCode, _, errno := syscall.Syscall(syscall.SYS_IOCTL,
		uintptr(syscall.Stdin),
		uintptr(syscall.TIOCGWINSZ),
		uintptr(unsafe.Pointer(ws)))

	if int(retCode) == -1 {
		panic(errno)
	}
	return int(ws.Row)
}

func Libraries_utils_term_prim__setupTerm(w any) any {
	return nil
}
