@.str = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1

declare i32 @printf(i8*, ...) #1

define i32 @main() {
  %r0 = add i32 0, 1
  %r1 = add i32 0, 1
  %r2 = mul i32 %r0, %r1
  %r3 = add i32 0, 1
  %r4 = mul i32 %r2, %r3
  %r5 = add i32 0, 1
  %r6 = mul i32 %r4, %r5
  call i32 (i8*, ...) @printf(i8* getelementptr ([4 x i8], [4 x i8]* @.str, i32 0, i32 0), i32 %r6)
  ret i32 0
}
