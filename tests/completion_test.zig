pub fn Funky(comptime T: type) type {
    return struct {
        pub fn bruh() ?T {
            return null;
        }
    };
}

pub fn main() void {
    const Z = Funky(u8);
    _ = Z;
}
