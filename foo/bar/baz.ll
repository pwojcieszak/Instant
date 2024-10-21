@.str = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1

declare i32 @printf(i8*, ...) #1

define i32 @main() {
  %a = alloca i32
  %r0 = add i32 0, 1
  store i32 %r0, i32* %a
  %b = alloca i32
  %r1 = add i32 0, 2
  store i32 %r1, i32* %b
  %r2 = load i32, i32* %b
  %r3 = load i32, i32* %a
  %r4 = load i32, i32* %a
  %r5 = add i32 0, 1
  %r6 = load i32, i32* %a
  %r7 = load i32, i32* %a
  %r8 = load i32, i32* %a
  %r9 = load i32, i32* %a
  %r10 = load i32, i32* %a
  %r11 = add i32 0, 1
  %r12 = load i32, i32* %a
  %r13 = load i32, i32* %a
  %r14 = load i32, i32* %a
  %r15 = load i32, i32* %b
  %r16 = add i32 0, 1
  %r17 = load i32, i32* %a
  %r18 = load i32, i32* %a
  %r19 = load i32, i32* %a
  %r20 = load i32, i32* %a
  %r21 = load i32, i32* %a
  %r22 = load i32, i32* %a
  %r23 = load i32, i32* %a
  %r24 = load i32, i32* %a
  %r25 = load i32, i32* %a
  %r26 = load i32, i32* %a
  %r27 = add i32 0, 1
  %r28 = load i32, i32* %a
  %r29 = load i32, i32* %a
  %r30 = load i32, i32* %a
  %r31 = load i32, i32* %a
  %r32 = add i32 0, 1
  %r33 = load i32, i32* %a
  %r34 = load i32, i32* %a
  %r35 = add i32 0, 1
  %r36 = load i32, i32* %a
  %r37 = load i32, i32* %a
  %r38 = add i32 0, 1
  %r39 = load i32, i32* %a
  %r40 = load i32, i32* %b
  %r41 = add i32 %r39, %r40
  %r42 = add i32 %r38, %r41
  %r43 = add i32 %r37, %r42
  %r44 = add i32 %r36, %r43
  %r45 = add i32 %r35, %r44
  %r46 = add i32 %r34, %r45
  %r47 = add i32 %r33, %r46
  %r48 = add i32 %r32, %r47
  %r49 = add i32 %r31, %r48
  %r50 = add i32 %r30, %r49
  %r51 = add i32 %r29, %r50
  %r52 = add i32 %r28, %r51
  %r53 = add i32 %r27, %r52
  %r54 = add i32 %r26, %r53
  %r55 = add i32 %r25, %r54
  %r56 = add i32 %r24, %r55
  %r57 = add i32 %r23, %r56
  %r58 = add i32 %r22, %r57
  %r59 = add i32 %r21, %r58
  %r60 = add i32 %r20, %r59
  %r61 = add i32 %r19, %r60
  %r62 = add i32 %r18, %r61
  %r63 = add i32 %r17, %r62
  %r64 = add i32 %r16, %r63
  %r65 = add i32 %r15, %r64
  %r66 = add i32 %r14, %r65
  %r67 = add i32 %r13, %r66
  %r68 = add i32 %r12, %r67
  %r69 = add i32 %r11, %r68
  %r70 = add i32 %r10, %r69
  %r71 = add i32 %r9, %r70
  %r72 = add i32 %r8, %r71
  %r73 = add i32 %r7, %r72
  %r74 = add i32 %r6, %r73
  %r75 = add i32 %r5, %r74
  %r76 = add i32 %r4, %r75
  %r77 = add i32 %r3, %r76
  %r78 = add i32 %r2, %r77
  call i32 (i8*, ...) @printf(i8* getelementptr ([4 x i8], [4 x i8]* @.str, i32 0, i32 0), i32 %r78)
  ret i32 0
}
