const std = @import("std");

const Package = @This();

name: []const u8,
path: []const u8,
/// if std is included in this list, it overrides the default std path
dependencies: std.ArrayListUnmanaged(Package),
