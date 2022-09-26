a: u8,
b: i16,
c: (u8),
d: (i16),
e: (p: {
    // const z: type = u8;
    if (1 == 1) {
        break :p u32;
    } else {
        break :p u8;
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
