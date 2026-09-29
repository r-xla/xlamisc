# xlamisc 0.5.0

* breaking: xlamisc now contains the former tengen package: the tensor
  generics (`shape()`, `dtype()`, `device()`, `as_array()`, `as_raw()`,
  `naxes()`, `nelts()`) and the `DataType` enum with its helpers. Use
  `xlamisc::` instead of `tengen::`.
* breaking: removed `LRUCache`, `get_dims()`, `without()`, `seq_len0()`,
  `seq_along0()`, `shapevec_repr()`, `shapevec_reprs()`, `new_list_of()`,
  `format_bib()` and `cite_bib()`. The ones still in use now live in the
  packages that use them.

# xlamisc 0.4.1

* `new_list_of()` no longer causes `R CMD check` to report "no visible global
  function definition for 'validator'" in packages that store the returned
  constructors.

# xlamisc 0.4.0

* `cite_bib()` now lists all authors, e.g. `"A & B (Year)"` or
  `"A et al. (Year)"`.
* Minimum required R version is now 4.3.0.

# xlamisc 0.3.0

* Fix Rd formatting
* new_list_of() no longer checks class of elements
  due to performance costs.

# xlamisc 0.2.0

* Added `shapevec_repr()` and `shapevec_reprs()` for formatting shape vectors.
* Added `without()` for removing elements from a vector.
* Added `format_bib()` for formatting bibliography entries.

# xlamisc 0.1.0

* Initial release
