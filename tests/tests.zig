const std = @import("std");
pub const bubbles = @import("bubbles.zig");

test {
    std.testing.refAllDecls(@This());

    try std.testing.expectEqual((u3), std.meta.fields(bubbles)[std.meta.fields(bubbles).len - 1].field_type);

    var a: usize = 1;

    (z: {
        break :z &a;
    }).* = 12;
}
