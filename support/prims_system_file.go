package support

import (
	"bufio"
	"io"
	"io/fs"
	"os"
	"os/exec"
	"syscall"
	"unsafe"
)

type filePtr struct {
	file      *os.File
	cmd       *exec.Cmd
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
		file:   os.Stdout,
		writer: bufio.NewWriter(os.Stdout),
	}
	stderr = &filePtr{
		file:   os.Stderr,
		writer: bufio.NewWriter(os.Stderr),
	}
)

func System_file_error_prim__fileErrno(w any) int {
	world := GetWorld(w)
	switch world.lastError {
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

	switch world.lastError.(type) {
	case *fs.PathError:
		return 2 // FileNotFound
	}

	return 5 // GenericFileError
}

func System_file_handle_prim__close(f, w any) any {
	world := GetWorld(w)
	ptr := f.(*filePtr)
	if ptr.writer != nil {
		ptr.writer.Flush()
	}
	if ptr.file != nil {
		err := ptr.file.Close()
		ptr.lastError = err
		world.SetLastError(err)
	}
	return nil
}

func System_file_handle_prim__open(f, m, w any) *filePtr {
	world := GetWorld(w)
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
		world.SetLastError(err)
		return nil
	}
	world.lastError = nil
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
	world := GetWorld(w)
	ptr := f.(*filePtr)
	data, err := ptr.reader.ReadByte()
	ptr.lastError = err
	world.SetLastError(err)
	if err != nil {
		return -1
	}
	return int(data)
}

func System_file_readwrite_prim__readChars(m, f, w any) *string {
	world := GetWorld(w)
	max := m.(int)
	ptr := f.(*filePtr)
	data := make([]byte, 0, max)
	_, err := ptr.reader.Read(data)
	ptr.lastError = err
	world.SetLastError(err)
	if err != nil {
		if err != io.EOF {
			return nil
		}
		ptr.eof = true
	}
	line := string(data)
	return &line
}

func System_file_readwrite_prim__readLine(f, w any) *string {
	world := GetWorld(w)
	ptr := f.(*filePtr)
	line, err := ptr.reader.ReadString('\n')
	ptr.lastError = err
	world.SetLastError(err)
	if err != nil {
		if err != io.EOF {
			return nil
		}
		ptr.eof = true
	}
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
	world := GetWorld(w)
	ptr := f.(*filePtr)
	_, err := ptr.writer.WriteString(l.(string))
	ptr.lastError = err
	world.SetLastError(err)
	if err != nil {
		return 0
	}
	return 1
}

func System_file_buffer_prim__readBufferData(f, b, o, m, w any) int {
	world := GetWorld(w)
	offset := o.(int)
	buffer := b.(Buffer)[offset : offset+m.(int)]
	ptr := f.(*filePtr)
	n, err := ptr.reader.Read(buffer)
	ptr.lastError = err
	world.SetLastError(err)
	if err != nil {
		return -1
	}
	return n
}

func System_file_buffer_prim__writeBufferData(f, b, o, s, w any) int {
	world := GetWorld(w)
	offset := o.(int)
	buffer := b.(Buffer)[offset : offset+s.(int)]
	ptr := f.(*filePtr)
	n, err := ptr.writer.Write(buffer)
	ptr.lastError = err
	world.SetLastError(err)
	if err != nil {
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
	ptr := f.(*filePtr)
	if ptr.file == nil { // if this is a pipe for a command
		return 0
	}
	info, _ := ptr.file.Stat()
	return int(info.ModTime().Unix())
}

func System_file_meta_prim__fileSize(f, w any) any {
	ptr := f.(*filePtr)
	if ptr.file == nil {
		return -1
	}
	info, err := ptr.file.Stat()
	if err != nil {
		panic(err)
	}
	return int(info.Size())
}

func System_file_permissions_prim__chmod(f, p, w any) int {
	world := GetWorld(w)
	err := os.Chmod(f.(string), os.FileMode(p.(int)))
	world.SetLastError(err)
	if err != nil {
		return 1
	}
	return 0
}

func System_file_process_prim__flush(f, w any) int {
	world := GetWorld(w)
	ptr := f.(*filePtr)
	if ptr.writer == nil {
		return 0
	}
	err := ptr.writer.Flush()
	ptr.lastError = err
	world.SetLastError(err)
	if err != nil {
		return 1
	}
	return 0
}

func System_file_process_prim__pclose(f, w any) int {
	ptr := f.(*filePtr)
	if ptr.cmd == nil {
		System_file_handle_prim__close(ptr, w)
		if ptr.lastError != nil {
			return -1
		}
		return 0
	}
	err := ptr.cmd.Wait()
	if err != nil {
		if exitErr, ok := err.(*exec.ExitError); ok {
			return exitErr.ProcessState.ExitCode()
		}
		return -1
	}
	return 0
}

func System_file_process_prim__popen(c, m, w any) *filePtr {
	world := GetWorld(w)
	mode := m.(string)
	read, write := false, false
	switch mode {
	case "r", "rb":
		read = true
	case "r+", "rb+":
		read = true
		write = true
	case "w", "wb", "a", "ab":
		write = true
	case "w+", "wb+", "a+", "ab+":
		read = true
		write = true
	}
	cmd := exec.Command("sh", "-c", c.(string))
	ptr := filePtr{
		file: nil,
		cmd:  cmd,
	}
	if write {
		w, err := cmd.StdinPipe()
		if err != nil {
			world.SetLastError(err)
			return nil
		}
		ptr.writer = bufio.NewWriter(w)
	}
	if read {
		r, err := cmd.StdoutPipe()
		if err != nil {
			world.SetLastError(err)
			return nil
		}
		ptr.reader = bufio.NewReader(r)
	}

	err := ptr.cmd.Start()
	if err != nil {
		world.SetLastError(err)
		return nil
	}
	return &ptr
}

func System_file_readwrite_prim__removeFile(f, w any) int {
	world := GetWorld(w)
	fname := f.(string)
	err := os.Remove(fname)
	world.SetLastError(err)
	if err != nil {
		return 1
	}
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

type winsize struct {
	Row    uint16
	Col    uint16
	Xpixel uint16
	Ypixel uint16
}

func system_file_meta_prim__file_winsz(fptr uintptr) (*winsize, error) {
	ws := &winsize{}
	retCode, _, errno := syscall.Syscall(syscall.SYS_IOCTL,
		fptr,
		uintptr(syscall.TIOCGWINSZ),
		uintptr(unsafe.Pointer(ws)))

	if int(retCode) == -1 {
		return nil, errno
	}
	return ws, nil
}

func System_file_meta_prim__fileIsTTY(f, w any) int {
	ptr := f.(*filePtr)

	_, err := system_file_meta_prim__file_winsz(ptr.file.Fd())
	if err != nil {
		return 0
	}
	return 1
}
