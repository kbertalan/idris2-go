package support

import "runtime"

func Main_system_info_prim__os() any {
	switch runtime.GOOS {
	case "windows":
		return "windows"
	case "darwin":
		return "darwin"
	default:
		return "unix"
	}
}

func Main_system_info_prim__codegen() string {
	return "go"
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

func Main_prelude_uninhabited_prim__void(a, w any) any {
	panic("executed void")
}
