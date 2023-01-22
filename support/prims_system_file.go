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
	writer    *bufio.Writer
	eof       bool
	lastError error
}

var (
	stdin = &filePtr{
		file:   os.Stdin,
		reader: bufio.NewReader(os.Stdin),
	}
	stdout = &filePtr{
		file: os.Stdout,
	}
	stderr = &filePtr{
		file: os.Stderr,
	}
)

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
	if filePtr.writer != nil {
		filePtr.writer.Flush()
	}
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
	createWriter := false
	switch m.(string) {
	case "r", "rb":
		mode |= os.O_RDONLY
		createReader = true
	case "w", "wb":
		mode |= os.O_WRONLY | os.O_TRUNC | os.O_CREATE
		createWriter = true
	case "a", "ab":
		mode |= os.O_WRONLY | os.O_APPEND
		createWriter = true
	case "r+", "rb+":
		mode |= os.O_RDWR
		createReader = true
		createWriter = true
	case "w+", "wb+":
		mode |= os.O_RDWR | os.O_TRUNC | os.O_CREATE
		createReader = true
		createWriter = true
	case "a+", "ab+":
		mode |= os.O_RDWR | os.O_APPEND
		createReader = true
		createWriter = true
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
	if createWriter {
		ptr.writer = bufio.NewWriter(file)
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

func System_file_readwrite_prim__readChar(f, w any) int {
	world := w.(*WorldType)
	ptr := f.(*filePtr)
	data, err := ptr.reader.ReadByte()
	if err != nil {
		ptr.lastError = err
		world.lastFileInError = ptr
	}
	ptr.lastError = nil
	world.lastFileInError = nil
	return int(data)
}

func System_file_readwrite_prim__readChars(m, f, w any) *string {
	world := w.(*WorldType)
	max := m.(int)
	ptr := f.(*filePtr)
	data := make([]byte, 0, max)
	_, err := ptr.reader.Read(data)
	if err != nil {
		if err != io.EOF {
			ptr.lastError = err
			world.lastFileInError = ptr
			return nil
		}
		ptr.eof = true
	}
	ptr.lastError = nil
	world.lastFileInError = nil
	line := string(data)
	return &line
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
	_, err := ptr.writer.WriteString(l.(string))
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
	n, err := ptr.writer.Write(buffer)
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

func System_file_process_prim__flush(f, w any) int {
	world := w.(*WorldType)
	ptr := f.(*filePtr)
	if ptr.writer == nil {
		return 0
	}
	err := ptr.writer.Flush()
	if err != nil {
		ptr.lastError = err
		world.lastFileInError = ptr
		return 1
	}
	return 0
}

func System_file_process_prim__pclose(f, w any) any   { panic("not implemented") }
func System_file_process_prim__popen(f, m, w any) any { panic("not implemented") }

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
	return stdin
}

func System_file_virtual_prim__stdout() *filePtr {
	return stdout
}

func System_file_virtual_prim__stderr() *filePtr {
	return stderr
}
