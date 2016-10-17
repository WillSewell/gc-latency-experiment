package main

import (
	"fmt"
	"time"
)

type message []byte

type channel map[int]message

const (
	windowSize = 200000
	msgCount   = 1000000
)

var worst time.Duration

func mkMessage(n int) message {
	m := make(message, 1024)
	for i := range m {
		m[i] = byte(n)
	}
	return m
}

func pushMsg(c *channel, highID int) {
	start := time.Now()
	lowID := highID - windowSize
	(*c)[highID] = mkMessage(highID)
	if lowID >= 0 {
		delete(*c, lowID)
	}
	elapsed := time.Since(start)
	if elapsed > worst {
		worst = elapsed
	}
}

func main() {
	c := make(channel)
	for i := 0; i < msgCount; i++ {
		pushMsg(&c, i)
	}
	fmt.Println("Worst push time: ", worst)
}
