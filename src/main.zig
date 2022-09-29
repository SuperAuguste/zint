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
    _ = try unit.interpret(0, null, .{ .observe_values = true, .is_comptime = false });

    // for (unit.type_info.items) |ti| {
    //     std.log.info("{any}", .{ti});
    // }

    // for (unit.fields.items) |field| {
    //     // std.log.info("{s}: {s}", args: anytype);
    //     std.log.info("{s}: {any}", .{ field.name, unit.formatType(field.@"type") });
    // }

    // for (unit.declarations.items) |decl| {
    //     // std.log.info("{s}: {s}", args: anytype);
    //     std.log.info("{s}: {any}; {any}", .{ decl.name, unit.formatType(decl.@"type"), decl.value.value_data });
    // }
}
