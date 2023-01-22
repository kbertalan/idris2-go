package support

func System_concurrency_prim__channelGet(a, ch, world any) any {
	return <-ch.(chan any)
}

func System_concurrency_prim__channelPut(a, ch, v, world any) any {
	ch.(chan any) <- v
	return nil
}

func System_concurrency_prim__makeChannel(a, world any) chan any {
	return make(chan any)
}
