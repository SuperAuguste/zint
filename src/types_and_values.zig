const std = @import("std");
const zig = std.zig;
const Ast = zig.Ast;
const utils = @import("utils.zig");

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

    pub const Pointer = struct {
        size: Size,
        is_const: bool,
        is_volatile: bool,
        child: Type,
        is_allowzero: bool,

        sentinel: ?ValueData,

        pub const Size = enum {
            one,
            many,
            slice,
            c,
        };
    };

    @"type",
    @"bool",

    @"struct": Struct,
    pointer: Pointer,

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

    // @"struct": struct {

    // },
    // one_ptr: *anyopaque,
    /// TODO: Optimize this with an ArrayList that uses anyopaque slice
    slice_ptr: std.ArrayListUnmanaged(ValueData),

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

    pub fn isConstant(declaration: Declaration, tree: Ast) bool {
        return switch (tree.nodes.items(.tag)[declaration.node_idx]) {
            .global_var_decl,
            .local_var_decl,
            .aligned_var_decl,
            .simple_var_decl,
            => {
                return tree.tokenSlice(utils.varDecl(tree, declaration.node_idx).?.ast.mut_token).len == 3;
            },
            else => false,
        };
    }
};
