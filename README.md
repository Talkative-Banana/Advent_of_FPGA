# Secret Entrance

## Blog Post
[Medium](https://medium.com/@lakshay21059/secret-entrance-74d65a2e5e45)

## Running the Project

This project is written in **OCaml** and uses **Hardcaml**.  
It is built and executed using **Dune**.

---

## Prerequisites

- OCaml (recommended via `opam`)
- Dune
- Hardcaml
- Hardcaml-Cyclesim

---

## Setup

Clone the repository and move into the project directory:

Install required dependencies:

opam install dune hardcaml hardcaml-cyclesim

---

## Build

Navigate to the `problem1` directory and build the project:  
```
cd problem1  
dune build
```

## Run (Sequential Version)
Run the sequential implementation:  
```
dune exec ./main.exe
```

---
## Run (Parallel Version)
Run the parallel implementation:  
```
dune exec ./tangent.exe <input_filename>
```
---
