#include <stdio.h>
#include <strings.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

CAMLprim value ml_ffs(value x) {
    return Val_int(ffsl(Long_val(x)));
}

CAMLprim value ml_ffs_intrinsic(value x) {
    return Val_int(__builtin_ffsl(Long_val(x)));
}

CAMLprim value ml_ffs_print(value x) {
    printf("%016lx %2d %2d    %016lx %2d %2d\n", Long_val(x), ffsl(Long_val(x)), ffs(Long_val(x)), (long) Int_val(x), ffsl(Int_val(x)), ffs(Int_val(x)));
    return Val_int(ffsl(Long_val(x)));
}
