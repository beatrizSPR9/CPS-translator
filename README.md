# CPS-translator

An automated translator that converts **OCaml programs written in A-normal form (ANF)** into **Continuation Passing Style (CPS)**. 

The repository includes example translations for pairing heaps, skew heaps, and tree height.

---

### Build

``` dune build ```

---

### Usage

``` dune exec bin/main.exe test/<filename> ```

---

### Project Structure

```
bin/    executable entry point 
lib/    translation logic 
test/   example programs 
```
