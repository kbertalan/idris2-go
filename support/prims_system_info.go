package support

import "runtime"

func System_info_prim__getNProcessors(w any) int {
	return runtime.NumCPU()
}
