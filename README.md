Address-Finder
==============

Address Finder is a package for matching textual addresses.
Several similarity scoring methods are implemented in this package.
The methods implemented in this package are targeting non-uniform textual addresses which may contain misspelling, 
missing words, etc.
The package can be used with Spark.

Textual Address similarity
--------------------------
The following similarity measures are defined in this package.

 - `LetterPrefixDistance` measures the length of common prefix in letters
 - `WordPrefixDistance` measures the length of common prefix in words
 - `NumberOverlapDistance` measures the differences of numbers appearing the addresses
 - `NumberSeqDistance` measures the numbers difference by Levenshtein distance
 - `PriorityWordDistance` measures word differences and assign more weights in the difference if the word appears at the
 beginning of the string
 - `StrictNumberOverlapDistance` a combination of number overlap and number seq differences with binary results
 - `WordBagDistance` measures the difference in word bag (word counts)
 - `WordSetDistance` measures the difference in words without considering of the numbers of appearance
 - `SymmetricWordSetDistance` similar to `WordSetDistance` and is symmetric
 - `SymmetricWordSetWithIDF` similar to `SymmetricWordSetDistance` and words are weighted by IDF in the collection
 
Searcher Composition
--------------------

Searchers can be composed with different components.
Please refer `uk.ac.cdrc.data.utility.text.AddressSearcher` for an example of search composition.
 
Quick Example
-------------

```scala
import uk.ac.cdrc.data.utility.text.AddressSearcher
val addressSetA = IndexedSeq(
    "1 some street some city", 
    "2 some street some city")
val addressSetB = IndexedSeq(
    "2 some street some city", 
    "2a some street some city")
val as = AddressSearcher(addressSetA)
val matching = for {a <- addressSetB
     r <- as search a
     if !r.multiTops
    } yield (r.top, a)
```
