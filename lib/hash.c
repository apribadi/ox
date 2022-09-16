#include <stdint.h>
#include <string.h>

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

/*
static inline uint64_t load(char * p, size_t n) {
  uint64_t a = 0;
  for (size_t i = 0; i < n; ++ i)
    a |= (uint64_t)(uint8_t) p[i] << i * 8;
  return a;
}
*/

static inline uint64_t load(char * p) {
  uint64_t a = 0;
  memcpy(&a, p, 8);
  return a;
}

static inline uint64_t rotl(uint64_t a, size_t b) {
  return (a << b) | (a >> (64 - b));
}

static inline uint64_t rotr(uint64_t a, size_t b) {
  return (a >> b) | (a << (64 - b));
}

static inline uint64_t mix1(uint64_t a) {
  a ^= rotl(a, 21) ^ rotr(a, 21);
  a *= 0x9999999999999999ull;
  return a;
}

static inline uint64_t mix2(uint64_t a) {
  a ^= a >> 7;
  a ^= a << 9;
  return a;
}

static inline uint64_t mix3(uint64_t a) {
  a = __builtin_bswap64(a);
  a *= 0x9999999999999999ull;
  return a;
}

static inline uint64_t hash(char * p, size_t n) {
  if (n == 0) return 0;

  size_t k = ((n - 1) & 7) + 1;

  // We might read *before the start* of the string, which is ok because OCaml
  // strings have a word-sized header.

  uint64_t h = mix1(load(p + n - 8) >> 8 * (8 - k));
  n -= k;

  while (n != 0) {
    h = mix2(h) + mix1(load(p + n - 8));
    n -= 8;
  }

  return mix3(h);
}

CAMLprim int64_t ox_hash_untagged_unboxed(value t, intnat pos, intnat len) {
  return hash((char *) t + pos, len);
}

CAMLprim value ox_hash(value t, value pos, value len) {
  CAMLparam3 (t, pos, len);
  CAMLlocal1 (r);

  r =
    caml_copy_int64(
      ox_hash_untagged_unboxed(
        t,
        Long_val(pos), 
        Long_val(len)
      )
    );

  CAMLreturn (r);
}
