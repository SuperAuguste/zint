const std = @import("std");
const zig = std.zig;
const Ast = zig.Ast;

pub const TypeInfo = union(enum) {
    pub const Signedness = enum { signed, unsigned };

    @"type",
    @"struct": struct {
        fields: std.ArrayListUnmanaged(Field),
        declarations: std.ArrayListUnmanaged(Declaration),
    },
    int: struct {
        bits: u16,
        signedness: Signedness,
    },
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
};

pub const ValueData = union(enum) {
    // TODO: Support larger ints, floats; bigints?

    @"type": Type,

    unsigned_int: u64,
    signed_int: i64,
    float: f64,
};

pub const Field = struct {
    node_idx: Ast.Node.Index,
    container_scope_idx: usize,
    @"type": Type,
    // default_value: ?Value,
};

pub const Declaration = struct {
    node_idx: Ast.Node.Index,
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
