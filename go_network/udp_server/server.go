package main

import (
	"fmt"
	"net"
)

func main() {
	// Listen for incoming UDP packets on port 8080.
	conn, err := net.ListenPacket("udp", ":8080")
	if err != nil {
		fmt.Println(err)
		return
	}
	defer conn.Close()

	buffer := make([]byte, 1024)
	// Read the incoming packet data into the buffer.
	n, addr, err := conn.ReadFrom(buffer)
	if err != nil {
		fmt.Println(err)
		return
	}
	fmt.Println("Received: ", string(buffer[:n]))
	// Write a response to the client's address.
	conn.WriteTo([]byte("Message received!"), addr)
}
