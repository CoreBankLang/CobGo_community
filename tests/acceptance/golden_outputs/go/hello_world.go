// Generated from DSL: hello_world.cobgo
// Golden dataset for acceptance testing

package golden

import (
	"fmt"
)

func HelloWorldMain() {
	// Initialize variables
	greetingMessage := "Hello, World!"
	counter := int32(0)

	// Execute main step
	helloWorldStep(greetingMessage, &counter)
}

func helloWorldStep(greetingMessage string, counter *int32) {
	// Display greeting message
	fmt.Println(greetingMessage)

	// Increment counter
	*counter = *counter + 1

	// Display counter
	fmt.Printf("Counter: %d\n", *counter)
}
