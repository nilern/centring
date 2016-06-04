# Components of Value

Conceptually, every Centring value has two parts:

1. Type -- a value of type Type
2. Data -- other values or some other bag of bytes

## Data Representation

### Bytes Types

* The data is a bag of bytes
* For builtins, FFI, compact "structs"
* FFI => Manage by non-moving GC

### Record Types

* The data is an array of ValueRefs
* Can use any GC, probably a copying one

## Types
