a: u8,
b: i16,
c: (u8),
d: (i16),
e: (p: {
    var joe: type = u69;
    var mama = u420;
    mama = u3;
    if (1 == 0) {
        break :p joe;
    } else {
        break :p mama;
    }
}),

// comptime {
//     a: {
//         b: {
//             ...
//             break :a
//             ...
//         }
//     }
// }

// const a: u8 = 1;
// const b: (u8) = 1;
// const c: (p: {
//     break :p u8;
// }) = 1;
