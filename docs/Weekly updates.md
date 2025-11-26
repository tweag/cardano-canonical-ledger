# Weekly status updates

## Week of 2025-11-24 — 2025-11-26

**Key accomplishments**

-Finished implementing all namespaces; implemented decoders and made cardano-ledger compile.

-Adapted many PRs and test harnesses; started adapting to cardano-ledger[HEAD] and verifying with scls-historical.

-Prepared CDDL PRs for committee namespace and proposals; continued improving encoder/decoder integration.

-Worked on ToCanonicalCBOR adaptation to ensure canonical map encoding (by encoded-key byte order).

-Continued versioned encoder stabilization and deterministic CBOR testing.

-Added manifest comment & timestamp support; profiled scls-util verify.

**Next steps**

-Finalize PRs for canonical encoding and deterministic CBOR rules; prepare patches and reviews.

-Run full end-to-end generation + verification using cardano-ledger[HEAD] and cardano-ledger[scls-historical].

-Integrate finalized metadata manifest decisions (comment/timestamp) into tooling and specs.

-Finalize site integration and tests for all metadata/namespace changes.


## Week of 2025-11-17 — 2025-11-23

**Key accomplishments**

-Sent PR for snapshot/v0 CDDL and prepared example project site.

-Finished many namespaces and progressed toward generating a full multi-namespace SCLS file.

-Implemented test code for committee/proposals namespaces; updated specs where needed.

-Continued refactor of versioned encoding/serialization API and implemented example end-to-end flow to validate design choices.

-Worked on ensuring versioned encoder applies key-size restrictions correctly.

**Next steps**

-Implement processing for namespace v0 on cardano-ledger-old and iterate to next namespaces.

-Adapt testing code to latest cardano-canonical-ledger master; attempt generating a full file with all namespaces.

-Decide on deterministic CBOR approach (implement on our side vs upstream cborg).

## Week of 2025-11-10 — 2025-11-16

**Key accomplishments**

-Prepared and updated PRs for multiple gov-related namespaces (pots, plotStake, gov/pparams).

-Created testing setups to generate SCLS files from older ledger branches and validate against modern checks.

-Implemented and tested several namespace encoders; addressed many PR comments.

-Continued work on the versioned encoder interface and serialization-plan refactor.

-Advanced the versioned-namespace encoder PR (type-level guarantees for key size).

**Next steps**

-Continue and finish snapshot namespace work (large effort).

-Finalize and merge versioned encoder PR; document design and interface.

-Run more end-to-end validation with scls-historical and modern tooling.

## Week of 2025-11-03 — 2025-11-09

**Key accomplishments**

-Updated and merged multiple PRs; ran scls-util check on generated files (found failures to address).

-Fixed encoding bugs and updated UTxO spec helpers; improved scls-cddl debug helpers.

-Started working on namespace encoders (pots, plotStake etc.) and generated SCLS test files for pots/UTxO.

-Continued PR reviews and started migration work from binary → mempack.

-Began work on versioned namespace encoder and serialization plan.

**Next steps**

-Improve scls-format and scls-check debug output; iterate on failing checks.

-Continue implementing namespace encoders and tests (simple namespaces first).

-Finish mempack migration and finalize versioned encoder PRs.


## Week of 20/10 – 23/10

**Key Accomplishments:**

-Namespace encoding issues fixed; SCLS integrated with cardano-cli.

-DumpConfig and serialization plan finalized.

-Metadata implementation updated; PR prepared.

-Profiling performed on scls-util verify function; Kaitai Struct spec updated.

**Next Steps:** Complete integration with Cardano CLI and ledger and to continue testing and optimize serialization/metadata handling.

## Week of 09/10 – 16/10

**Key Accomplishments:**

-Split/merge commands implemented for SCLS files.

-Public interface for SCLS format designed and integrated.

-Metadata serialization updated to align with chunk entry changes.

-Extract feature implemented and tested; documentation completed.

**Next Steps** : Run end-to-end testing of split/merge and extract features and to continue ledger integration and demo preparation.

## Week of 22/09 – 26/09

**Key Accomplishments:**

-PRs reviewed; binary specification for SCLS files prepared.

-Nix flake setup updated for multiple dev shells.

-Metadata record structure implemented and tested.

-Decoders for UTxO namespace implemented; CIP specification compliance verified.

**Next Steps:** Plan next steps for Cardano ledger integration and to address issues discovered during metadata and decoder testing.

## Week of 15/09 – 19/09

**Key Accomplishments:**

-Multiple namespace support completed for external sort.

-ChunksBuilder tests and documentation finalized.

-External sort implementation verified with tests.

-PRs reviewed and merged for Merkle tree integration.

**Next Steps:** Continue polishing namespace code and tests and to Begin integration with higher-level ledger tooling.


## Week of 08/09 – 12/09

 **Key Accomplishments:**
 -CI updated for all GHC versions.
 
 -Reference serializer improved; tombstone clarifications added.
 
 -Merkle tree fully integrated; roundtrip tests passing.
 
 -Initial namespace support implemented for external sort.
 
 -Started CBOR serialization for ledger integration; tested UTxO decoders.
 
**Next Steps:**
  -Complete namespace and multi-file support.
 
 -Finalize CBOR-based ledger integration.
 
 -Resolve duplicate logic and compatibility issues with Cardano ledger.
 
## Week of 03/09 – 05/09
 **Key Accomplishments:**
 
 -Updated CIP and fixed link styles.
 
 -Implemented basic serializer and started CDDL for UTxOs.
 
 -Finalized Nix PR and validated Merkle root PoC.
 
 -Prepared initial serialization context (Merkle tree, block digest, chunk allocation).
 
**Next Steps:**

 -Complete serializer integration.
 
 -Address remaining CIP comments.
 
 -Proceed with ledger showcase integration.

