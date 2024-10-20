define i32 @main() {
  %a = alloca i32
  %r0 = add i32 0, 1
  store i32 %r0, i32* %a
  %b = alloca i32
  %r1 = add i32 0, 2
  store i32 %r1, i32* %b
  %r2 = load i32, i32* %b
  %r3 = load i32, i32* %a
  %r4 = add i32 %r2, %r3
  ret i32 %r4
}
