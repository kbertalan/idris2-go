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
	var slice []E
	for value := v.(Value); value.Tag == 1; value = value.Args[1].(Value) {
		slice = append(slice, value.Args[0].(E))
	}
	return slice
}

func Go2IdrisSlice[E any](v []E) Value {
	value := Constructor(0)
	for i := len(v) - 1; i >= 0; i-- {
		value = Constructor(1, v[i], value)
	}
	return value
}
