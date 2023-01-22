package support

import "fmt"

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
