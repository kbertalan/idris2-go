package support

import (
	"fmt"
	"time"
)

func System_clock_prim__clockTimeGcCpu(world any) any  { panic("not implemented") }
func System_clock_prim__clockTimeGcReal(world any) any { panic("not implemented") }

func System_clock_prim__clockTimeMonotonic(world any) time.Time {
	// go's time is monotonic between 1885 and 2157 years
	return time.Now()
}

var processStartTime = time.Now()

func System_clock_prim__clockTimeProcess(world any) time.Duration {
	return time.Since(processStartTime)
}

func System_clock_prim__clockTimeThread(w any) time.Duration {
	world := w.(*WorldType)
	return time.Since(world.threadStartTime)
}

func System_clock_prim__clockTimeUtc(world any) time.Time {
	return time.Now().UTC()
}

func System_clock_prim__osClockNanosecond(v, world any) uint64 {
	switch t := v.(type) {
	case time.Time:
		return uint64(t.Nanosecond())
	case time.Duration:
		return uint64(t % 1e9)
	}
	panic(fmt.Sprintf("unknown time type %T", v))
}

func System_clock_prim__osClockSecond(v, world any) uint64 {
	switch t := v.(type) {
	case time.Time:
		return uint64(t.Unix())
	case time.Duration:
		return uint64(t / 1e9)
	}
	panic(fmt.Sprintf("unknown time type %T", v))
}

func System_clock_prim__osClockValid(v, world any) any { panic("not implemented") }
