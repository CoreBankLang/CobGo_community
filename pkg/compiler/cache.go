package compiler

import (
	"crypto/sha256"
	"encoding/gob"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"time"

	"github.com/cobgo/cobgo-community/pkg/ir"
	"github.com/cobgo/cobgo-community/pkg/parser"
)

// Cache manages compilation artifacts to speed up incremental builds
type Cache struct {
	cacheDir string
	enabled  bool
	stats    CacheStats
}

// CacheStats tracks cache statistics
type CacheStats struct {
	Hits   int
	Misses int
	Errors int
}

// CachedProgram represents a cached compilation result
type CachedProgram struct {
	SourceHash    string
	AST           *parser.Program
	IR            *ir.Program
	CompiledAt    time.Time
	SourceModTime time.Time
	Dependencies  []string
}

// NewCache creates a new compilation cache
func NewCache(cacheDir string) *Cache {
	return &Cache{
		cacheDir: cacheDir,
		enabled:  true,
	}
}

// Enable enables the cache
func (c *Cache) Enable() {
	c.enabled = true
}

// Disable disables the cache
func (c *Cache) Disable() {
	c.enabled = false
}

// Initialize creates the cache directory if it doesn't exist
func (c *Cache) Initialize() error {
	if !c.enabled {
		return nil
	}

	if err := os.MkdirAll(c.cacheDir, 0755); err != nil {
		return fmt.Errorf("failed to create cache directory: %w", err)
	}

	return nil
}

// Get retrieves a cached program if available and valid
func (c *Cache) Get(sourceFile string) (*CachedProgram, error) {
	if !c.enabled {
		c.stats.Misses++
		return nil, fmt.Errorf("cache disabled")
	}

	// Check if source file exists and get its hash
	sourceHash, err := hashFile(sourceFile)
	if err != nil {
		c.stats.Errors++
		return nil, fmt.Errorf("failed to hash source file: %w", err)
	}

	// Get cache file path
	cacheFile := c.getCacheFilePath(sourceHash)

	// Check if cache file exists
	if _, err := os.Stat(cacheFile); os.IsNotExist(err) {
		c.stats.Misses++
		return nil, fmt.Errorf("cache miss")
	}

	// Read cached program
	cached, err := c.readCacheFile(cacheFile)
	if err != nil {
		c.stats.Errors++
		return nil, fmt.Errorf("failed to read cache: %w", err)
	}

	// Verify source hash matches
	if cached.SourceHash != sourceHash {
		c.stats.Misses++
		return nil, fmt.Errorf("cache invalidated: source hash mismatch")
	}

	// Check source file modification time
	sourceInfo, err := os.Stat(sourceFile)
	if err != nil {
		c.stats.Errors++
		return nil, fmt.Errorf("failed to stat source file: %w", err)
	}

	if sourceInfo.ModTime().After(cached.CompiledAt) {
		c.stats.Misses++
		return nil, fmt.Errorf("cache invalidated: source modified")
	}

	c.stats.Hits++
	return cached, nil
}

// Put stores a compiled program in the cache
func (c *Cache) Put(sourceFile string, ast *parser.Program, ir *ir.Program) error {
	if !c.enabled {
		return nil
	}

	// Get source hash
	sourceHash, err := hashFile(sourceFile)
	if err != nil {
		return fmt.Errorf("failed to hash source file: %w", err)
	}

	// Get source modification time
	sourceInfo, err := os.Stat(sourceFile)
	if err != nil {
		return fmt.Errorf("failed to stat source file: %w", err)
	}

	// Create cached program
	cached := &CachedProgram{
		SourceHash:    sourceHash,
		AST:           ast,
		IR:            ir,
		CompiledAt:    time.Now(),
		SourceModTime: sourceInfo.ModTime(),
		Dependencies:  []string{}, // TODO: Track dependencies
	}

	// Write to cache
	cacheFile := c.getCacheFilePath(sourceHash)
	if err := c.writeCacheFile(cacheFile, cached); err != nil {
		c.stats.Errors++
		return fmt.Errorf("failed to write cache: %w", err)
	}

	return nil
}

// Invalidate removes a cached entry
func (c *Cache) Invalidate(sourceFile string) error {
	if !c.enabled {
		return nil
	}

	sourceHash, err := hashFile(sourceFile)
	if err != nil {
		return fmt.Errorf("failed to hash source file: %w", err)
	}

	cacheFile := c.getCacheFilePath(sourceHash)

	if err := os.Remove(cacheFile); err != nil && !os.IsNotExist(err) {
		return fmt.Errorf("failed to remove cache file: %w", err)
	}

	return nil
}

// Clear removes all cached entries
func (c *Cache) Clear() error {
	if !c.enabled {
		return nil
	}

	if err := os.RemoveAll(c.cacheDir); err != nil {
		return fmt.Errorf("failed to clear cache: %w", err)
	}

	return c.Initialize()
}

// GetStats returns cache statistics
func (c *Cache) GetStats() CacheStats {
	return c.stats
}

// HitRate returns the cache hit rate as a percentage
func (c *Cache) HitRate() float64 {
	total := c.stats.Hits + c.stats.Misses
	if total == 0 {
		return 0
	}
	return float64(c.stats.Hits) / float64(total) * 100
}

// getCacheFilePath returns the cache file path for a source hash
func (c *Cache) getCacheFilePath(sourceHash string) string {
	return filepath.Join(c.cacheDir, sourceHash+".cache")
}

// readCacheFile reads a cached program from file
func (c *Cache) readCacheFile(cacheFile string) (*CachedProgram, error) {
	file, err := os.Open(cacheFile)
	if err != nil {
		return nil, err
	}
	defer file.Close()

	decoder := gob.NewDecoder(file)
	var cached CachedProgram
	if err := decoder.Decode(&cached); err != nil {
		return nil, fmt.Errorf("failed to decode cache: %w", err)
	}

	return &cached, nil
}

// writeCacheFile writes a cached program to file
func (c *Cache) writeCacheFile(cacheFile string, cached *CachedProgram) error {
	file, err := os.Create(cacheFile)
	if err != nil {
		return err
	}
	defer file.Close()

	encoder := gob.NewEncoder(file)
	if err := encoder.Encode(cached); err != nil {
		return fmt.Errorf("failed to encode cache: %w", err)
	}

	return nil
}

// hashFile computes the SHA-256 hash of a file
func hashFile(filepath string) (string, error) {
	file, err := os.Open(filepath)
	if err != nil {
		return "", err
	}
	defer file.Close()

	hash := sha256.New()
	if _, err := io.Copy(hash, file); err != nil {
		return "", err
	}

	return fmt.Sprintf("%x", hash.Sum(nil)), nil
}

// DependencyGraph tracks compilation dependencies
type DependencyGraph struct {
	nodes map[string]*DependencyNode
}

// DependencyNode represents a file in the dependency graph
type DependencyNode struct {
	FilePath     string
	Dependencies []string
	Dependents   []string
	LastModified time.Time
}

// NewDependencyGraph creates a new dependency graph
func NewDependencyGraph() *DependencyGraph {
	return &DependencyGraph{
		nodes: make(map[string]*DependencyNode),
	}
}

// AddDependency adds a dependency relationship
func (dg *DependencyGraph) AddDependency(file string, dependency string) {
	if _, exists := dg.nodes[file]; !exists {
		dg.nodes[file] = &DependencyNode{
			FilePath:     file,
			Dependencies: []string{},
			Dependents:   []string{},
		}
	}

	if _, exists := dg.nodes[dependency]; !exists {
		dg.nodes[dependency] = &DependencyNode{
			FilePath:     dependency,
			Dependencies: []string{},
			Dependents:   []string{},
		}
	}

	dg.nodes[file].Dependencies = append(dg.nodes[file].Dependencies, dependency)
	dg.nodes[dependency].Dependents = append(dg.nodes[dependency].Dependents, file)
}

// GetDependencies returns all dependencies of a file
func (dg *DependencyGraph) GetDependencies(file string) []string {
	if node, exists := dg.nodes[file]; exists {
		return node.Dependencies
	}
	return []string{}
}

// GetDependents returns all files that depend on this file
func (dg *DependencyGraph) GetDependents(file string) []string {
	if node, exists := dg.nodes[file]; exists {
		return node.Dependents
	}
	return []string{}
}

// GetInvalidatedFiles returns files that need recompilation due to changes
func (dg *DependencyGraph) GetInvalidatedFiles(changedFile string) []string {
	invalidated := make(map[string]bool)
	dg.collectInvalidated(changedFile, invalidated)

	files := make([]string, 0, len(invalidated))
	for file := range invalidated {
		files = append(files, file)
	}

	return files
}

// collectInvalidated recursively collects invalidated files
func (dg *DependencyGraph) collectInvalidated(file string, invalidated map[string]bool) {
	if invalidated[file] {
		return
	}

	invalidated[file] = true

	for _, dependent := range dg.GetDependents(file) {
		dg.collectInvalidated(dependent, invalidated)
	}
}
