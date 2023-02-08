package support

import (
	"time"
)

type WorldType struct {
	lastError       error
	threadStartTime time.Time
}

func NewWorld() *WorldType {
	return &WorldType{
		lastError:       nil,
		threadStartTime: time.Now(),
	}
}

func GetWorld(w any) *WorldType {
	switch w.(type) {
	case *WorldType:
		return w.(*WorldType)
	default: // assuming the value was erased
		return nil
	}
}

func (w *WorldType) Fork() *WorldType {
	world := WorldType{}
	if w != nil {
		world = *w
	}
	world.lastError = nil
	world.threadStartTime = time.Now()
	return &world
}

func (w *WorldType) SetLastError(err error) {
	if w != nil {
		w.lastError = err
	}
}

func (w *WorldType) GetLastError() error {
	if w != nil {
		return w.lastError
	}
	return nil
}

func (w *WorldType) GetThreadStartTime() time.Time {
	if w != nil {
		return w.threadStartTime
	}
	return time.Time{}
}

func Idris2GoSlice[E any](v any) []E {
	vec := v.(Vector)
	slice := make([]E, 0, vec.Len())
	for iter := vec.ReverseIter(); iter.HasNext(); {
		slice = append(slice, iter.Next().(E))
	}
	return slice
}
