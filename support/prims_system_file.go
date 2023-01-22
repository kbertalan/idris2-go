package support

import (
	"bufio"
	"io"
	"io/fs"
	"os"
)

type filePtr struct {
	file      *os.File
	reader    *bufio.Reader
	eof       bool
	lastError error
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

func System_file_buffer_prim__readBufferData(f, b, o, m, w any) int {
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

func System_file_buffer_prim__writeBufferData(f, b, o, s, w any) int {
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
