@.str = private unnamed_addr constant [3 x i8] c"%d\00", align 1
declare i32 @printf(ptr noundef, ...) #1
attributes #0 = { noinline nounwind optnone uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }

define void @main() {
  sub i32 44, 2
  call i32 (ptr, ...) @printf(ptr noundef @.str, i32 noundef %1)
  ret void
}
