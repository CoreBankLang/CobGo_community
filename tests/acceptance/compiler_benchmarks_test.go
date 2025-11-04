package acceptance

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"strings"
	"testing"
)

// BenchmarkCompilerPipeline benchmarks the full compilation pipeline
func BenchmarkCompilerPipeline(b *testing.B) {
	// Create a simple DSL test file
	dslContent := `job test_job {
    var counter int64 = 0
    var result decimal = 0.0
    step main {
        while (counter < 100) {
            result = result + 1.0
            counter = counter + 1
        }
        display("Result:", result)
    }
}`

	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		// Create temporary DSL file
		tmpDSL, err := os.CreateTemp("", "benchmark_*.cobgo")
		if err != nil {
			b.Fatal(err)
		}
		defer os.Remove(tmpDSL.Name())
		tmpDSL.WriteString(dslContent)
		tmpDSL.Close()

		// Create temporary Go output file
		tmpGo, err := os.CreateTemp("", "benchmark_*.go")
		if err != nil {
			b.Fatal(err)
		}
		defer os.Remove(tmpGo.Name())
		tmpGo.Close()

		// Run the compiler
		cmd := exec.Command("go", "run", "../../cmd/dslc", "-i", tmpDSL.Name(), "-o", tmpGo.Name())
		var stderr bytes.Buffer
		cmd.Stderr = &stderr
		if err := cmd.Run(); err != nil {
			b.Fatalf("Compilation failed: %v, stderr: %s", err, stderr.String())
		}
	}
}

// BenchmarkParser benchmarks the parser component
func BenchmarkParser(b *testing.B) {
	// Create a simple DSL test file
	dslContent := `job test_job {
    var counter int64 = 0
    var result decimal = 0.0
    step main {
        while (counter < 100) {
            result = result + 1.0
            counter = counter + 1
        }
        display("Result:", result)
    }
}`

	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		// Create temporary DSL file
		tmpDSL, err := os.CreateTemp("", "benchmark_*.cobgo")
		if err != nil {
			b.Fatal(err)
		}
		defer os.Remove(tmpDSL.Name())
		tmpDSL.WriteString(dslContent)
		tmpDSL.Close()

		// Run the parser only (without code generation)
		cmd := exec.Command("go", "run", "../../cmd/dslc", "-i", tmpDSL.Name(), "-o", os.DevNull)
		var stderr bytes.Buffer
		cmd.Stderr = &stderr
		if err := cmd.Run(); err != nil {
			b.Fatalf("Parsing failed: %v, stderr: %s", err, stderr.String())
		}
	}
}

// BenchmarkCodeGeneration benchmarks the code generation component
func BenchmarkCodeGeneration(b *testing.B) {
	// Create a simple DSL test file
	dslContent := `job test_job {
    var counter int64 = 0
    var result decimal = 0.0
    step main {
        while (counter < 100) {
            result = result + 1.0
            counter = counter + 1
        }
        display("Result:", result)
    }
}`

	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		// Create temporary DSL file
		tmpDSL, err := os.CreateTemp("", "benchmark_*.cobgo")
		if err != nil {
			b.Fatal(err)
		}
		defer os.Remove(tmpDSL.Name())
		tmpDSL.WriteString(dslContent)
		tmpDSL.Close()

		// Create temporary Go output file
		tmpGo, err := os.CreateTemp("", "benchmark_*.go")
		if err != nil {
			b.Fatal(err)
		}
		defer os.Remove(tmpGo.Name())
		tmpGo.Close()

		// Run the full compiler
		cmd := exec.Command("go", "run", "../../cmd/dslc", "-i", tmpDSL.Name(), "-o", tmpGo.Name())
		var stderr bytes.Buffer
		cmd.Stderr = &stderr
		if err := cmd.Run(); err != nil {
			b.Fatalf("Code generation failed: %v, stderr: %s", err, stderr.String())
		}
	}
}

// BenchmarkLargeProgram benchmarks compilation of a larger program
func BenchmarkLargeProgram(b *testing.B) {
	// Create a larger DSL test file
	var dslContent strings.Builder
	dslContent.WriteString("job large_program {\n")

	// Add many variables
	for i := 0; i < 100; i++ {
		dslContent.WriteString(fmt.Sprintf("    var var%d int64 = %d\n", i, i))
	}

	// Add many variables
	for i := 0; i < 50; i++ {
		dslContent.WriteString(fmt.Sprintf("    var decimal%d decimal = %d.0\n", i, i))
	}

	dslContent.WriteString("    step main {\n")
	dslContent.WriteString("        var counter int64 = 0\n")
	dslContent.WriteString("        while (counter < 50) {\n")

	// Add complex operations
	for i := 0; i < 20; i++ {
		dslContent.WriteString(fmt.Sprintf("            var%d = var%d + 1\n", i, i))
	}

	for i := 0; i < 10; i++ {
		dslContent.WriteString(fmt.Sprintf("            decimal%d = decimal%d + 1.0\n", i, i))
	}

	dslContent.WriteString("            counter = counter + 1\n")
	dslContent.WriteString("        }\n")
	dslContent.WriteString("        display(\"Program completed\")\n")
	dslContent.WriteString("    }\n")
	dslContent.WriteString("}\n")

	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		// Create temporary DSL file
		tmpDSL, err := os.CreateTemp("", "benchmark_large_*.cobgo")
		if err != nil {
			b.Fatal(err)
		}
		defer os.Remove(tmpDSL.Name())
		tmpDSL.WriteString(dslContent.String())
		tmpDSL.Close()

		// Create temporary Go output file
		tmpGo, err := os.CreateTemp("", "benchmark_large_*.go")
		if err != nil {
			b.Fatal(err)
		}
		defer os.Remove(tmpGo.Name())
		tmpGo.Close()

		// Run the compiler
		cmd := exec.Command("go", "run", "../../cmd/dslc", "-i", tmpDSL.Name(), "-o", tmpGo.Name())
		var stderr bytes.Buffer
		cmd.Stderr = &stderr
		if err := cmd.Run(); err != nil {
			b.Fatalf("Large program compilation failed: %v, stderr: %s", err, stderr.String())
		}
	}
}

// BenchmarkConcurrentCompilation benchmarks concurrent compilation
func BenchmarkConcurrentCompilation(b *testing.B) {
	// Create a simple DSL test file
	dslContent := `job test_job {
    var counter int64 = 0
    step main {
        while (counter < 10) {
            counter = counter + 1
        }
        display("Counter:", counter)
    }
}`

	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		// Create multiple temporary DSL files for concurrent compilation
		done := make(chan bool, 5)

		for j := 0; j < 5; j++ {
			go func(id int) {
				// Create temporary DSL file
				tmpDSL, err := os.CreateTemp("", fmt.Sprintf("benchmark_concurrent_%d_*.cobgo", id))
				if err != nil {
					done <- false
					return
				}
				defer os.Remove(tmpDSL.Name())
				tmpDSL.WriteString(dslContent)
				tmpDSL.Close()

				// Create temporary Go output file
				tmpGo, err := os.CreateTemp("", fmt.Sprintf("benchmark_concurrent_%d_*.go", id))
				if err != nil {
					done <- false
					return
				}
				defer os.Remove(tmpGo.Name())
				tmpGo.Close()

				// Run the compiler
				cmd := exec.Command("go", "run", "../../cmd/dslc", "-i", tmpDSL.Name(), "-o", tmpGo.Name())
				var stderr bytes.Buffer
				cmd.Stderr = &stderr
				if err := cmd.Run(); err != nil {
					done <- false
					return
				}

				done <- true
			}(j)
		}

		// Wait for all goroutines to complete
		for j := 0; j < 5; j++ {
			if !<-done {
				b.Fatal("Concurrent compilation failed")
			}
		}
	}
}
