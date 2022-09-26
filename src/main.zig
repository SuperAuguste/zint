const std = @import("std");
const Package = @import("Package.zig");
const SourceUnit = @import("SourceUnit.zig");

pub fn main() !void {
    const allocator = std.heap.page_allocator;

    var package = Package{
        .name = "root",
        // .path = "C:/Programming/Zig/zig-from-the-website/lib/std/std.zig",
        .path = "C:/Programming/Zig/zig-interpreter/tests/bubbles.zig",
        .dependencies = .{},
    };

    // TODO: Don't pass a local in!
    var unit = try SourceUnit.init(allocator, &package, package.path);
    _ = try unit.interpret(0, null, false);

    for (unit.fields.items) |field|
        std.log.info("{any}", .{unit.formatType(field.@"type")});
}
