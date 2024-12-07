# orange
Small and modular HTTP server written using OCaml and Lwt. It supports customizable routing, middleware, and testing.

## Usage
```
dune exec ./src/main.ml
```
By default, the server listens on `http://localhost:8080`

## Routes

The server includes predefined routes:

```
GET /hello
Response: "Hello, World!"
``
and:
```
GET /goodbye
Response: "Goodbye, World!"
```
