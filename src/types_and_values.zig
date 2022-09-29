const std = @import("std");
const zig = std.zig;
const Ast = zig.Ast;

pub const TypeInfo = union(enum) {
    pub const Signedness = enum { signed, unsigned };

    pub const Struct = struct {
        /// Index into unit.fields
        fields: std.ArrayListUnmanaged(usize) = .{},
        /// Index into declarations.fields
        declarations: std.ArrayListUnmanaged(usize) = .{},
    };

    pub const Int = struct {
        bits: u16,
        signedness: Signedness,
    };

    @"type",
    @"bool",
    @"struct": Struct,
    int: Int,
    @"comptime_int",
    float: u16,
    @"comptime_float",
};

pub const Type = struct {
    node_idx: Ast.Node.Index,
    // TODO: Work on type deduplication strategy; memoization issues?
    info_idx: usize,
};

pub const Value = struct {
    node_idx: Ast.Node.Index,
    @"type": Type,
    value_data: ValueData,

    pub fn eql(value: Value, other_value: Value) bool {
        return value.value_data.eql(other_value.value_data);
    }
};

pub const ValueData = union(enum) {
    // TODO: Support larger ints, floats; bigints?

    @"type": Type,
    @"bool": bool,

    @"comptime_int": std.math.big.int.Managed,
    unsigned_int: u64,
    signed_int: i64,
    float: f64,

    pub fn eql(data: ValueData, other_data: ValueData) bool {
        if (std.meta.activeTag(data) != std.meta.activeTag(other_data)) return false;
        // std.enums.
        // std.meta.activeTag(u: anytype)
        switch (data) {
            .@"bool" => return data.@"bool" == other_data.@"bool",
            .@"comptime_int" => return data.@"comptime_int".eq(other_data.@"comptime_int"),
            .unsigned_int => return data.unsigned_int == other_data.unsigned_int,
            .signed_int => return data.signed_int == other_data.signed_int,
            .float => return data.float == other_data.float,
            else => @panic("Simple eql not implemented!"),
        }
    }
};

pub const Field = struct {
    node_idx: Ast.Node.Index,
    /// Store name so tree doesn't need to be used to access field name
    name: []const u8,
    container_scope_idx: usize,
    @"type": Type,
    // default_value: ?Value,
};

pub const Declaration = struct {
    node_idx: Ast.Node.Index,
    /// Store name so tree doesn't need to be used to access declaration name
    name: []const u8,
    scope_idx: usize,
    @"type": Type,
    value: Value,

    // TODO: figure this out
    // pub const DeclarationKind = enum{variable, function};
    // pub fn declarationKind(declaration: Declaration, tree: Ast) DeclarationKind {
    //     return switch(tree.nodes.items(.tag)[declaration.node_idx]) {
    //         .fn_proto,
    //     .fn_proto_one,
    //     .fn_proto_simple,
    //     .fn_proto_multi,
    //     .fn_decl
    //     }
    // }
};
