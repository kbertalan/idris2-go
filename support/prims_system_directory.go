package support

import (
	"os"
)

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
	w.(*WorldType).lastError = nil
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
	w.(*WorldType).lastError = nil
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
