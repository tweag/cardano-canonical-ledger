# SCLS Merkle Tree Library

A Haskell library providing incremental Merkle tree construction for the Cardano Canonical Ledger State (SCLS) project. This library enables efficient Merkle tree building without requiring all data to be available upfront.

## Installation and Usage

Add `scls-merkle-tree` as a dependency in your `cabal.project` or `.cabal` file:

```cabal
build-depends: scls-merkle-tree
```

### Basic Usage

The library is parameterized by any `HashAlgorithm` from the `crypton` library, allowing you to choose the hashing function:

```haskell
import Cardano.SCLS.MerkleTree (getMerkleHash, merkleTreeHash)
import Cardano.SCLS.MerkleTree.Constructor
import Crypto.Hash.Algorithms (SHA3_256, Blake2b_160)
import qualified Data.ByteString.Char8 as C

let constructor = add (empty :: Constructor SHA3_256) (C.pack "entry1")
let constructor' = add constructor (C.pack "entry2")
let merkleTree = finalize constructor'
let rootHash = getMerkleHash $ merkleTreeHash merkleTree

-- Or use a different hash algorithm
let constructor = add (empty :: Constructor Blake2b_160) (C.pack "entry1")
```

## Algorithm and Approach

This library implements an **incremental constructor** that builds Merkle trees on-the-fly as entries are added, using a stack-based approach to maintain partial tree states.

### Key Components

1. **Incremental Constructor**: A stack of `ConstructorNode` elements, each containing a level and hash
2. **Pluggable Hash Algorithms**: Parameterized by any `HashAlgorithm` from the `crypton` library (SHA256, SHA3-256, BLAKE2b, etc.)
3. **Level-based Joining**: Nodes at the same level are automatically combined to build the tree bottom-up, thus minimizing memory usage

### Algorithm Details

The constructor maintains a stack of partial tree nodes, where each node has:

- `cLevel`: The height/level in the tree (0 for leaves)
- `cHash`: The hash value at that node

**Adding Entries**:

1. Each new entry is hashed to create a leaf node at level 0
2. The new leaf is pushed onto the constructor stack
3. The `join` function immediately combines adjacent nodes at the same level
4. This process continues recursively until no more joins are possible

**Joining Logic**:

- When two nodes at the same level exist consecutively, they are combined
- The combined node moves up one level and gets a new hash computed from the two child hashes

**Finalization**:

- If the constructor is empty, returns an empty Merkle tree
- If only one node remains, it becomes the root
- If multiple nodes exist, continues joining nodes at equal levels until convergence

## API Reference

The library exposes two main modules:

- `Cardano.SCLS.MerkleTree`: Core Merkle tree types and operations
- `Cardano.SCLS.MerkleTree.Constructor`: Incremental constructor functionality

### Testing and Verification

The library includes a property-based test using QuickCheck that verifies the incremental constructor produces identical results to a reference batch Merkle tree implementation from the [`merkle-tree`](https://hackage.haskell.org/package/merkle-tree) library.

Run tests with:

```bash
cabal test scls-merkle-tree-test
```

### Space Complexity

The algorithm has **O(log n)** space complexity where n is the number of entries added.

The constructor maintains a stack of `ConstructorNode` elements, where each node represents a partial tree at a specific level. The key insight is that at any given time, there can be at most one node per level in the stack.

**Analysis:**

- When you add entries, the stack can have at most ⌊log₂(n)⌋ + 1 nodes
- Each level k can contain at most one partial tree representing 2^k leaf nodes  
- The maximum level needed for n entries is ⌊log₂(n)⌋
- Therefore, the stack size is bounded by O(log n)

**Example with 7 entries:**

- After adding 4 entries: stack might have [Level2Node] (representing 4 leaves)
- After adding 2 more: [Level2Node, Level1Node] (4 + 2 leaves)
- After adding 1 more: [Level2Node, Level0Node, Level0Node] then joins to [Level2Node, Level1Node]

The `join` function ensures that adjacent nodes at the same level are immediately combined, preventing the stack from growing beyond O(log n) size.
