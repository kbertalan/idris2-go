package support

import (
	"syscall"
)

func System_errno_prim__strerror(e, world any) any {
	errno := syscall.Errno(e.(int))
	return errno.Error()
}

func System_errno_prim__getErrno(w any) any {
	world := w.(*WorldType)
	if world.lastError != nil {
		return 1 // TODO find out what errno-s can be extracted from errors
	}
	return 0
}
