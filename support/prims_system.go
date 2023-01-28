package support

import (
	"os"
	"os/exec"
	"time"
)

func System_prim__exit(code, world any) any {
	os.Exit(code.(int))
	return nil
}

func System_prim__getArg(n, world any) any {
	return os.Args[n.(int)]
}

func System_prim__getArgCount(world any) any {
	return len(os.Args)
}

func System_prim__getEnv(v, world any) *string {
	value, ok := os.LookupEnv(v.(string))
	if !ok {
		return nil
	}
	return &value
}

func System_prim__getEnvPair(n, w any) *string {
	i := n.(int)
	env := os.Environ()
	if i >= 0 && i < len(env) {
		pair := env[i]
		return &pair
	}
	return nil
}

func System_prim__setEnv(n, v, o, w any) int {
	world := w.(*WorldType)
	name := n.(string)
	value := v.(string)
	overwrite := o.(int) != 0
	_, exists := os.LookupEnv(name)
	if exists && !overwrite {
		return 0
	}
	err := os.Setenv(name, value)
	if err != nil {
		world.lastError = err
		return 1
	}
	return 0
}

func System_prim__unsetEnv(n, w any) int {
	if err := os.Unsetenv(n.(string)); err != nil {
		world := w.(*WorldType)
		world.lastError = err
		return 1
	}
	return 0
}

func System_prim__system(v, w any) any {
	cmd := exec.Command("sh", "-c", v.(string))

	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	err := cmd.Run()
	if err != nil {
		if exitErr, ok := err.(*exec.ExitError); ok {
			return exitErr.ProcessState.ExitCode()
		}
		return -1
	}
	return 0
}

func System_prim__sleep(s, w any) any {
	sec := time.Duration(s.(int))
	time.Sleep(sec * time.Second)
	return nil
}

func System_prim__usleep(u, w any) any {
	usec := time.Duration(u.(int))
	time.Sleep(usec * time.Microsecond)
	return nil
}

func System_prim__time(w any) int {
	return int(time.Now().Unix())
}

func System_prim__getPID(w any) int {
	return os.Getpid()
}
