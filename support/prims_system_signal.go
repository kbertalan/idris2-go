package support

import (
	"fmt"
	"os"
	"os/signal"
	"syscall"
)

// enableSignal is a workaround which uses the fact that an ignored signal is enabled again when attaching a channel using signal.Notify.
// signal.Reset does not enable an ignored signal
func enableSignal(s os.Signal) {
	if signal.Ignored(s) {
		ch := make(chan os.Signal, 0)
		signal.Notify(ch, s)
		signal.Stop(ch)
		close(ch)
	}
}

func System_signal_prim__defaultSignal(s, w any) int {
	sig := syscall.Signal(s.(int))
	enableSignal(sig)
	signal.Reset(sig)
	return 0
}

func System_signal_prim__ignoreSignal(s, w any) int {
	signal.Ignore(syscall.Signal(s.(int)))
	return 0
}

var receivedSignals = make(chan os.Signal, 1)

func System_signal_prim__collectSignal(s, w any) int {
	signal.Notify(receivedSignals, syscall.Signal(s.(int)))
	return 0
}

// System_signal_prim__handleNextCollectedSignal is not guaranteeing the order of signals, also it does not perform deduplication
func System_signal_prim__handleNextCollectedSignal(w any) int {
	select {
	case s, ok := <-receivedSignals:
		if !ok {
			panic("signal channel is unexpectedly closed")
		}
		switch signal := s.(type) {
		case syscall.Signal:
			return int(signal)
		default:
			panic(fmt.Sprintf("unknown signal type %T with value %v", s, s))
		}
	default:
		return -1
	}
}

func System_signal_prim__raiseSignal(s, w any) int {
	world := w.(*WorldType)
	err := syscall.Kill(syscall.Getpid(), syscall.Signal(s.(int)))
	world.lastError = err
	return 0
}

func System_signal_prim__sigabrt() int {
	return int(syscall.SIGABRT)
}

func System_signal_prim__sigfpe() int {
	return int(syscall.SIGFPE)
}

func System_signal_prim__sighup() int {
	return int(syscall.SIGHUP)
}

func System_signal_prim__sigill() int {
	return int(syscall.SIGILL)
}

func System_signal_prim__sigint() int {
	return int(syscall.SIGINT)
}

func System_signal_prim__sigquit() int {
	return int(syscall.SIGQUIT)
}

func System_signal_prim__sigsegv() int {
	return int(syscall.SIGSEGV)
}

func System_signal_prim__sigtrap() int {
	return int(syscall.SIGTRAP)
}

func System_signal_prim__sigusr1() int {
	return int(syscall.SIGUSR1)
}

func System_signal_prim__sigusr2() int {
	return int(syscall.SIGUSR2)
}
